const path = require("path")
const fs = require("fs")
const glob = require('glob')

var parseArgs = require('minimist')

const argv = parseArgs(process.argv.slice(2));

const sourcePath = argv['source']

const config = {
  input: path.join(__dirname, sourcePath),
  output: path.join(__dirname, 'dist', 'found-names.json'),
}

// Delete the previous output file
// Temporary indicator of failure, since error handling isn't working correctly
try {
  fs.unlinkSync(config.output)
} catch (_) {
  // Don't error if the file isn't there
}

let worker, flags

try {
  worker = require('./dist/worker')
} catch (_) {
  console.error('No ./dist/worker.js file found. Make sure to run "npm run build" first!')
  process.exit(1)
}

try {
  const input = glob.sync(config.input + '/**/*.elm', {}).map(file => {
    return fs.readFileSync(file, { encoding: 'utf-8' })
  })
  flags = { files: input }
} catch (err) {
  console.error('Could not read file: ' + config.input + 'Error: ' + err)
  process.exit(1)
}

const app = worker.Elm.Worker.init({ flags })

app.ports.outgoing.subscribe(message => {
  const handler = handlers[message.tag]
  if (!handler) return console.warn('UNKNOWN', message)
  handler(message.payload)
})

const handlers = {
  'Ok': (payload) => {
    console.log('Read ' + config.input + ' successfully.')
    console.log('Processing output file...')
    try {
      fs.writeFileSync(config.output, JSON.stringify(payload, null, '  '), { encoding: 'utf-8' })
    } catch (err) {
      console.error("Couldn't write file. Error: " + err)
      process.exit(1)
    }
    console.log('Wrote ' + config.output + ' successfully.')
  },
  'Err': (payload) => console.warn('ERR', payload),
}
