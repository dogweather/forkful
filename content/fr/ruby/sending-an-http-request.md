---
title:                "Envoyer une requête http"
html_title:           "Bash: Envoyer une requête http"
simple_title:         "Envoyer une requête http"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/ruby/sending-an-http-request.md"
---

{{< edit_this_page >}}

## Quoi et Pourquoi ? 
L'envoi d'une requête HTTP, c'est parler au serveur pour obtenir/gérer des données. Les programmeurs le font pour interagir avec les APIs, sites web, etc.

## Comment faire :
Voici comment envoyer une requête GET en utilisant le gem 'net/http' de Ruby.

```Ruby
require 'net/http'
require 'uri'

uri = URI.parse("https://www.example.com/")

response = Net::HTTP.get_response(uri)

puts response.body
```

Et voici comment envoyer une requête POST.

```Ruby
require 'net/http'
require 'uri'

uri = URI.parse("https://www.example.com/")

http = Net::HTTP.new(uri.host, uri.port)
request = Net::HTTP::Post.new(uri.request_uri)

response = http.request(request)

puts response.body
```

## Plongée en profondeur
Initialement, pour envoyer des requêtes HTTP en Ruby, les gens utilisaient `Net::HTTP`. It vient avec Ruby, peut faire presque tout mais c'est verbose.

Il existe des alternatives plus faciles, telles que `Rest-client` et `HTTParty`. Ces gems fournissent une API plus agréable.

```Ruby
# Avec Rest-client
require 'rest-client'

puts RestClient.get('https://www.example.com')

# Avec HTTParty
require 'httparty'

puts HTTParty.get('https://www.example.com').body
```

Quand on envoie une requête HTTP, Ruby établit une connexion TCP, envoie une chaîne de texte formatée selon les règles HTTP, puis lit la réponse.

## Voir aussi
- [Bibliothèque standard Ruby: net/http (Anglais)](https://ruby-doc.org/stdlib-3.0.2/libdoc/net/http/rdoc/Net/HTTP.html)
- [Gem Rest-client (Anglais)](https://rubygems.org/gems/rest-client)
- [Gem HTTParty (Anglais)](https://rubygems.org/gems/httparty)