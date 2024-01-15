---
title:                "Envoyer une requête HTTP avec une authentification de base"
html_title:           "Ruby: Envoyer une requête HTTP avec une authentification de base"
simple_title:         "Envoyer une requête HTTP avec une authentification de base"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/ruby/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## Pourquoi 

Pour de nombreuses raisons, vous pourriez avoir besoin d'envoyer une requête HTTP avec une authentification de base en Ruby. Peut-être que vous voulez vous connecter à une API, accéder à une ressource protégée ou mettre en place une sécurité pour votre propre application. Quelle que soit votre motivation, il est important de savoir comment le faire correctement.

## Comment faire 

Pour envoyer une requête HTTP avec une authentification de base en Ruby, vous pouvez utiliser la bibliothèque `net/http`. Voici un exemple de code pour effectuer une requête GET avec une authentification de base:

```Ruby
require 'net/http'

uri = URI('https://www.example.com/api')
req = Net::HTTP::Get.new(uri)
req.basic_auth 'username', 'password'
res = Net::HTTP.start(uri.hostname, uri.port) {|http|
  http.request(req)
}
puts res.body
```

Le code ci-dessus crée une instance `Net::HTTP::Get` pour la ressource `https://www.example.com/api`, ajoute l'authentification de base avec le nom d'utilisateur et le mot de passe fournis, puis envoie la requête en utilisant `Net::HTTP.start`. La réponse est stockée dans la variable `res` et le corps de la réponse peut être accédé en utilisant `res.body`.

## Plongée en profondeur 

En utilisant la bibliothèque `net/http`, il est également possible de spécifier un autre type d'authentification en utilisant `req.basic_auth` avec une instance `Net::HTTP::DigestAuth` ou `Net::HTTP::TokenAuth`. Vous pouvez également modifier les en-têtes de la requête en utilisant `req.add_field` pour définir des en-têtes personnalisés. Il est important de noter que l'authentification de base est considérée comme une méthode de sécurité de base et ne doit pas être utilisée pour protéger des informations sensibles.

## Voir aussi 

- [Documentation officielle de `net/http`](https://ruby-doc.org/stdlib-2.6.3/libdoc/net/http/rdoc/Net/HTTP.html)
- [Tutoriel sur l'authentification de base en Ruby](https://www.rubyguides.com/2018/07/ruby-http-basic-authentication/)
- [Exemples pratiques d'utilisation de `net/http`](https://web-crunch.com/ruby-on-rails-net-http-cheat-sheet/)