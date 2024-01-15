---
title:                "Envoyer une requête http"
html_title:           "Ruby: Envoyer une requête http"
simple_title:         "Envoyer une requête http"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/ruby/sending-an-http-request.md"
---

{{< edit_this_page >}}

## Pourquoi

Vous vous demandez peut-être pourquoi vous devriez vous préoccuper d'envoyer une requête HTTP en utilisant Ruby. Eh bien, si vous souhaitez communiquer avec une API, récupérer des données à partir d'un service web ou poster des formulaires en ligne, alors vous avez besoin de savoir comment envoyer une requête HTTP.

## Comment faire

Envoyer une requête HTTP en utilisant Ruby est assez simple grâce à la bibliothèque standard `Net::HTTP`. Tout d'abord, vous devez créer une instance d'un objet `Net::HTTP` en spécifiant l'URL de la requête:

```Ruby
require 'net/http'

uri = URI('https://example.com/')
http = Net::HTTP.new(uri.host, uri.port)
```

Ensuite, choisissez la méthode de requête appropriée (GET, POST, PUT, DELETE, etc.) et spécifiez le chemin de l'URL:

```Ruby
request = Net::HTTP::Get.new(uri.path)
```

Vous pouvez également ajouter des en-têtes à votre requête si nécessaire:

```Ruby
request['Content-Type'] = 'application/json'
```

Si vous envoyez des données avec votre requête, vous devrez les ajouter à la requête en utilisant la méthode `#body=`:

```Ruby
request.body = { name: 'John', age: 30 }.to_json
```

Enfin, vous pouvez envoyer la requête en utilisant la méthode `#request` de l'objet `Net::HTTP` et récupérer la réponse:

```Ruby
response = http.request(request)
puts response.body
```

Et voilà, vous venez d'envoyer une requête HTTP en utilisant Ruby!

## Plongée en profondeur

Si vous souhaitez en savoir plus sur les requêtes HTTP, voici quelques informations supplémentaires:

- La méthode `#request` renvoie une instance de la classe `Net::HTTPResponse`, qui contient toutes les informations renvoyées par le serveur, comme le code de statut, les en-têtes et le corps de la réponse.
- Vous pouvez également spécifier des options supplémentaires lors de la création de votre objet `Net::HTTP`, telles que la limite de temps pour la réponse ou si vous voulez utiliser une connexion sécurisée.
- Si vous devez gérer des requêtes asynchrones, vous pouvez également utiliser la bibliothèque `Net::HTTP::Async`, qui vous permet d'envoyer plusieurs requêtes en même temps.

## Voir aussi

- [Documentation de Net::HTTP](https://ruby-doc.org/stdlib-2.7.2/libdoc/net/http/rdoc/Net/HTTP.html)
- [Exemples de requêtes HTTP en Ruby](https://www.codecademy.com/learn/learn-ruby/modules/learn-ruby-ruby-on-the-web-u/cheatsheet)