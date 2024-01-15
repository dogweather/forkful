---
title:                "Inviare una richiesta http"
html_title:           "Ruby: Inviare una richiesta http"
simple_title:         "Inviare una richiesta http"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/ruby/sending-an-http-request.md"
---

{{< edit_this_page >}}

## Perché

Mandare una richiesta HTTP è un'operazione comune nel mondo della programmazione moderna. Ciò consente ai programmatori di comunicare con i server e ottenere o inviare dati tramite il Web.

## Come Fare

Per mandare una richiesta HTTP in Ruby, è possibile utilizzare la gemma `net/http` integrata, che fornisce una classe `Net::HTTP` per gestire le richieste. Di seguito è riportato un esempio di codice che invia una richiesta GET a un URL specifico:

```Ruby
require 'net/http'

url = URI('https://www.example.com')
response = Net::HTTP.get_response(url)

puts response.code # Stampa il codice di stato della risposta (es. 200)
puts response.body # Stampa il corpo della risposta
```

Questo codice crea prima un'istanza di `URI` con l'URL desiderato e poi invia una richiesta GET a quel URL utilizzando il metodo `get_response` della classe `Net::HTTP`. Il risultato viene memorizzato nella variabile `response` e può essere utilizzato per ottenere informazioni sulla risposta, come il codice di stato e il corpo.

## Approfondimento

È possibile personalizzare ulteriormente la richiesta utilizzando altri metodi della classe `Net::HTTP`, ad esempio per inviare una richiesta di tipo POST con dei dati:

```Ruby
require 'net/http'

url = URI('https://www.example.com')
params = { username: 'John', password: 'password' } # Dati da inviare
response = Net::HTTP.post(url, params)

puts response.code # Stampa il codice di stato della risposta (es. 200)
puts response.body # Stampa il corpo della risposta
```

In questo caso, invece di utilizzare il metodo `get_response`, viene utilizzato il metodo `post` e i dati da inviare sono specificati come un hash nella variabile `params`.

È importante anche gestire eventuali errori o eccezioni che possono verificarsi durante l'invio della richiesta. Ad esempio, se un server non risponde o se l'URL è errato, è possibile gestire l'eccezione `Net::HTTPError` in modo da evitare che il programma si interrompa improvvisamente.

## Vedi Anche

- La documentazione ufficiale di `net/http`: https://ruby-doc.org/stdlib-2.7.1/libdoc/net/http/rdoc/Net/HTTP.html
- Un tutorial su come utilizzare `net/http`: https://rubyplus.com/articles/5001-HTTP-Get-Post-Request-in-Ruby