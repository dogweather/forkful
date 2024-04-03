---
date: 2024-01-20 18:00:30.941065-07:00
description: "Inviare una richiesta HTTP significa dialogare con un altro sistema\
  \ via web; \xE8 come chiedere a un sito di inviarti dei dati o di accettare i tuoi.\
  \ I\u2026"
lastmod: '2024-03-13T22:44:44.048317-06:00'
model: gpt-4-1106-preview
summary: "Inviare una richiesta HTTP significa dialogare con un altro sistema via\
  \ web; \xE8 come chiedere a un sito di inviarti dei dati o di accettare i tuoi."
title: Inviare una richiesta http
weight: 44
---

## Che Cosa e Perché?
Inviare una richiesta HTTP significa dialogare con un altro sistema via web; è come chiedere a un sito di inviarti dei dati o di accettare i tuoi. I programmatori lo fanno per interagire con API web, raccogliere informazioni, o inviare dati a un server.

## Come Fare:
```Ruby
require 'net/http'
require 'uri'

uri = URI('http://www.example.com/index.html')
response = Net::HTTP.get_response(uri)

puts response.body if response.is_a?(Net::HTTPSuccess)
```
Output:
```
<!doctype html>
...
</!doctype>
```

## Approfondimento:
Inviare richieste HTTP è una pratica comune fin dagli albori del web. `Net::HTTP` è la libreria standard di Ruby per HTTP ma non è l'unica opzione. Puoi usare gemme come `HTTParty` o `Faraday` per un'interfaccia più elegante e diverse funzionalità. `Net::HTTP` è sincrona (bloccante): il tuo programma aspetterà una risposta prima di continuare. Asincrono (non bloccante) è possibile con gemme come `EventMachine` o celluloid-IO.

## Vedi Anche:
- Ruby-Doc per `Net::HTTP`: https://ruby-doc.org/stdlib-3.0.0/libdoc/net/http/rdoc/Net/HTTP.html
- Documentazione `HTTParty`: https://github.com/jnunemaker/httparty
- Documentazione `Faraday`: https://lostisland.github.io/faraday/
