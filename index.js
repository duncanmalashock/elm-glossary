const path = require("path")
const fs = require("fs")

const config = {
  input: path.join(__dirname, 'src', 'Main.elm'),
  output: path.join(__dirname, 'dist', 'found-names.json'),
}

let worker, flags

try {
  worker = require('./dist/worker')
} catch (_) {
  console.error('Make sure to run "npm run build" first!')
  process.exit(1)
}

try {
  const input = fs.readFileSync(config.input, { encoding: 'utf-8' })
  flags = { file: input }
} catch (_) {
  console.error('Could not read the file')
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
      fs.writeFileSync(config.output, JSON.stringify(payload), { encoding: 'utf-8' })
    } catch (err) {
      console.error("Couldn't write file. Error: " + err)
      process.exit(1)
    }
    console.log('Wrote ' + config.output + ' successfully.')
  },
  'Err': (payload) => console.warn('ERR', payload),
}
