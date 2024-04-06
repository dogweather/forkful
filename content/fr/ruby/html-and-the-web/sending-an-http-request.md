---
date: 2024-01-20 18:00:38.666488-07:00
description: "How to: (Comment faire :) Ruby facilite l'envoi de requ\xEAtes HTTP\
  \ avec des biblioth\xE8ques comme Net::HTTP, qui vient int\xE9gr\xE9e, ou des gemmes\
  \ externes comme\u2026"
lastmod: '2024-04-05T21:53:59.821700-06:00'
model: gpt-4-1106-preview
summary: "(Comment faire :) Ruby facilite l'envoi de requ\xEAtes HTTP avec des biblioth\xE8\
  ques comme Net::HTTP, qui vient int\xE9gr\xE9e, ou des gemmes externes comme `httparty`."
title: "Envoi d'une requ\xEAte HTTP"
weight: 44
---

## How to: (Comment faire :)
Ruby facilite l'envoi de requêtes HTTP avec des bibliothèques comme Net::HTTP, qui vient intégrée, ou des gemmes externes comme `httparty`. Voici un exemple avec Net::HTTP :

```Ruby
require 'net/http'
require 'uri'

uri = URI('http://example.com/users')
response = Net::HTTP.get_response(uri)

puts response.body if response.is_a?(Net::HTTPSuccess)
```

Et avec `httparty` :

```Ruby
require 'httparty'

response = HTTParty.get('http://example.com/users')

puts response.body if response.success?
```

Sortie pour les deux exemples (supposant que le serveur envoie un JSON de utilisateurs) :

```Ruby
[
  {"id": 1, "name": "Alice"},
  {"id": 2, "name": "Bob"}
]
```

## Deep Dive (Plongée en profondeur)
Net::HTTP, c'est le standard de Ruby depuis la version 1.8, mais ça peut paraître verbeux pour des tâches plus simples. Des gemmes comme `httparty` ou `faraday` offrent une interface plus élégante et plus de fonctionnalités.

Lorsque Ruby est apparu en 1995, le web en était à ses balbutiements. Aujourd'hui, l'envoi de requêtes HTTP est une part fondamentale du développement web. En terme d'implémentation, une requête HTTP initie une communication avec un serveur web via le protocole HTTP, attend une réponse et traite cette réponse, généralement au format XML ou JSON.

## See Also (Voir Aussi)
- Documentation Ruby Net::HTTP : https://ruby-doc.org/stdlib-3.0.0/libdoc/net/http/rdoc/Net/HTTP.html
- gem `httparty` : https://github.com/jnunemaker/httparty
- gem `faraday` : https://lostisland.github.io/faraday/
- Introduction au protocole HTTP (Mozilla) : https://developer.mozilla.org/fr/docs/Web/HTTP/Overview
- JSON Parsing in Ruby : https://flaviocopes.com/ruby-json/
