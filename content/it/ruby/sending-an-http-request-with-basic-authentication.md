---
title:                "Inviare una richiesta http con autenticazione di base"
html_title:           "Bash: Inviare una richiesta http con autenticazione di base"
simple_title:         "Inviare una richiesta http con autenticazione di base"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/ruby/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

# Invio di una richiesta HTTP con autenticazione di base in Ruby
Ruby offre modi semplici e diretti per lavorare con le richieste HTTP. Vediamo come inviare una richiesta HTTP con autenticazione di base in Ruby.

## Che cos'è e Perché?
L'invio di una richiesta HTTP con autenticazione di base è un modo per comunicare con un server web che richiede le credenziali di autenticazione dell'utente. I programmatori lo fanno per interagire con le API o i server web che necessitano di controllo di accesso.

## Come fare:
Ruby offre la libreria 'net/http' per gestire le richieste HTTP. Di seguito è riportato un esempio su come inviare una richiesta HTTP con autenticazione di base.

```Ruby
require 'net/http'
require 'uri'

uri = URI('http://example.com')
req = Net::HTTP::Get.new(uri)
req.basic_auth 'user', 'pass'

res = Net::HTTP.start(uri.hostname, uri.port) {|http|
  http.request(req)
}
puts res.body
```
## Approfondimento
Storicamente, l'autenticazione di base HTTPS è stata introdotta nel protocollo HTTP nel 1996 come parte della specifica HTTP/1.0. Nonostante la sua età, è ancora ampiamente usata a causa della sua semplicità.

Ci sono varie alternative all'autenticazione di base HTTP come OAuth e autenticazione JWT, che forniscono un livello di sicurezza più elevato.

L'implementazione dell'autenticazione di base in Ruby è piuttosto semplice, come mostrato nel codice sopra. Tuttavia, è importante notare che le informazioni di autenticazione vengono inviate come testo non crittografato. Perciò, dovrebbe essere utilizzato solo su connessioni sicure come HTTPS.

## Vedi anche
Ruby offre diverse librerie per lavorare con le richieste HTTP. Qui ci sono alcuni link per ulteriori informazioni:

2. [HTTParty Gem](https://github.com/jnunemaker/httparty)
3. [Rest-Client Gem](https://github.com/rest-client/rest-client)

Ricorda, è importante scegliere l'approccio che meglio si adatta alle tue esigenze specifiche.