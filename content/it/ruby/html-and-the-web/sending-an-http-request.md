---
date: 2024-01-20 18:00:30.941065-07:00
description: "Come Fare: Inviare richieste HTTP \xE8 una pratica comune fin dagli\
  \ albori del web. `Net::HTTP` \xE8 la libreria standard di Ruby per HTTP ma non\
  \ \xE8 l'unica\u2026"
lastmod: '2024-04-05T21:53:44.693536-06:00'
model: gpt-4-1106-preview
summary: "Inviare richieste HTTP \xE8 una pratica comune fin dagli albori del web."
title: Inviare una richiesta http
weight: 44
---

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
