---
title:                "Envoyer une requête http avec une authentification de base"
html_title:           "Arduino: Envoyer une requête http avec une authentification de base"
simple_title:         "Envoyer une requête http avec une authentification de base"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/ruby/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

# Envoi d'une requête HTTP avec une authentification de base en Ruby

## Quoi & Pourquoi ?

L'envoi d'une requête HTTP avec une authentification de base est une façon de se connecter à des APIs sécurisées. Les programmeurs le font pour interagir avec des services en ligne, souvent pour récupérer des données.

## Comment faire :

La bibliothèque net/http intégrée à Ruby permet d'envoyer facilement des requêtes HTTP. Voici comment envoyer une requête GET avec une authentification de base :

```Ruby
require 'net/http'
require 'uri'

uri = URI('https://api.exemple.com/data')

req = Net::HTTP::Get.new(uri)
req.basic_auth 'nom_utilisateur', 'mot_de_passe'

res = Net::HTTP.start(uri.hostname, uri.port, :use_ssl => uri.scheme == 'https') {|http|
  http.request(req)
}

puts res.body
```

## Plongée profonde :

Historiquement, l'authentification HTTP de base est l'une des premières façons de sécuriser les connexions à des services en ligne. Cependant, elle n'est pas très sécurisée par elle-même et doit être utilisée en conjonction avec HTTPS.

L'authentification de base consiste à envoyer un nom d'utilisateur et un mot de passe non chiffrés avec chaque requête, donc si quelqu'un pouvait intercepter la requête, il pourrait voir vos identifiants. C'est pour cette raison que vous devez toujours utiliser HTTPS quand vous utilisez l'authentification de base.

Une alternative est l'authentification par jeton, qui est un peu plus sécurisée car elle ne nécessite pas l'envoi de vos identifiants à chaque fois.

## Voir aussi :

Pour plus d'informations sur l'utilisation de net/http dans Ruby, consultez la documentation officielle ici : [Ruby Doc](https://ruby-doc.org/stdlib-2.6.3/libdoc/net/http/rdoc/Net/HTTP.html)

Et pour une discussion sur la sécurité de l'authentification de base, voir cet article sur StackOverflow : [Basic Auth Security](https://stackoverflow.com/questions/1240518/is-basic-auth-secure-if-done-over-https)