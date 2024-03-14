---
date: 2024-01-20 18:02:31.480111-07:00
description: "Envoyer une requ\xEAte HTTP avec une authentification basique c\u2019\
  est ins\xE9rer vos identifiants dans une requ\xEAte pour acc\xE9der \xE0 des ressources\
  \ prot\xE9g\xE9es. Les\u2026"
lastmod: '2024-03-13T22:44:58.419661-06:00'
model: gpt-4-1106-preview
summary: "Envoyer une requ\xEAte HTTP avec une authentification basique c\u2019est\
  \ ins\xE9rer vos identifiants dans une requ\xEAte pour acc\xE9der \xE0 des ressources\
  \ prot\xE9g\xE9es. Les\u2026"
title: "Envoi d'une requ\xEAte HTTP avec authentification de base"
---

{{< edit_this_page >}}

## Quoi & Pourquoi?
Envoyer une requête HTTP avec une authentification basique c’est insérer vos identifiants dans une requête pour accéder à des ressources protégées. Les développeurs utilisent cela pour interagir avec des API qui exigent une preuve d'identité simple pour l'entrée.

## Comment faire :
```Ruby
require 'net/http'
require 'uri'

uri = URI('https://exemple.com/api')

# Create a request object
request = Net::HTTP::Get.new(uri)
request.basic_auth('user', 'password')

# Send the request
response = Net::HTTP.start(uri.hostname, uri.port, use_ssl: uri.scheme == 'https') do |http|
  http.request(request)
end

puts response.body
```
Sortie :
```
{"status":"Success","message":"Vous êtes authentifié!"}
```

## Exploration approfondie
Historiquement, l'authentification basique a été conçue pour être simple mais pas particulièrement sécurisée, car les identifiants ne sont encodés qu'en Base64, facilement décodable. Aujourd'hui, il est recommandé de l'utiliser avec HTTPS pour éviter une exposition claire des identifiants. 

Alternativement, des méthodes plus robustes comme l'authentification Digest ou OAuth ajoutent des couches de sécurité. En Ruby, on peut aussi utiliser des gems comme `HTTParty` ou `RestClient` pour simplifier la gestion des requêtes HTTP.

La mise en œuvre implique de créer un object HTTP::Request, d'ajouter les identifiants avec la méthode `basic_auth` et d'expédier la requête via une instance HTTP::Response. Il est essentiel de gérer les possibles erreurs de connexion et les réponses erronées.

## Voir aussi
- Ruby Documentation for Net::HTTP: https://ruby-doc.org/stdlib-3.0.0/libdoc/net/http/rdoc/Net/HTTP.html
- RFC 7617, The 'Basic' HTTP Authentication Scheme: https://tools.ietf.org/html/rfc7617
- An article on HTTP authentication schemes: https://developer.mozilla.org/en-US/docs/Web/HTTP/Authentication
