---
title:                "Ruby: Envoyer une demande http"
simple_title:         "Envoyer une demande http"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/ruby/sending-an-http-request.md"
---

{{< edit_this_page >}}

# Pourquoi

L'utilisation des requêtes HTTP est essentielle pour communiquer avec les différents serveurs sur le web. Cela permet à votre application de récupérer et d'envoyer des données telles que des fichiers, des images ou du texte.

# Comment faire

Pour envoyer une requête GET en Ruby, utilisez le module Net::HTTP et la méthode get. Voici un exemple de code :

```Ruby
require 'uri'
require 'net/http'

url = URI("https://www.example.com/")
response = Net::HTTP.get_response(url)

puts response.body if response.is_a?(Net::HTTPSuccess)
```

Dans cet exemple, nous importons les modules Net::HTTP et URI pour pouvoir utiliser la méthode get. Nous passons l'URL de la requête en tant qu'argument et stockons la réponse dans la variable `response`. Ensuite, nous vérifions si la réponse est valide en utilisant la méthode `is_a?` et si c'est le cas, nous imprimons le corps de la réponse.

# Plongée en profondeur

Il existe différents types de requêtes HTTP tels que GET, POST, PUT et DELETE. Chaque type a une fonction spécifique et peut également être associé à des en-têtes HTTP pour fournir plus d'informations sur la requête.

Par exemple, pour envoyer une requête POST avec un corps JSON, vous pouvez utiliser le code suivant :

```Ruby
require 'uri'
require 'net/http'
require 'json'

url = URI("https://www.example.com/")
payload = {"message": "Hello, world!"}

Net::HTTP.post(url, payload.to_json, "Content-Type": "application/json")
```

Nous utilisons ici la méthode `post` et nous spécifions le type de contenu du corps de la requête en tant qu'argument. De plus, nous nous assurons que les données sont au format JSON en convertissant notre hash en une chaîne JSON à l'aide de la méthode `to_json`.

# Voir aussi

- Documentation officielle de Net::HTTP : https://ruby-doc.org/stdlib-2.7.1/libdoc/net/http/rdoc/Net/HTTP.html
- Tutoriel sur les requêtes HTTP en Ruby : https://www.rubyguides.com/2018/07/ruby-http-request/
- Utilisation des en-têtes HTTP en Ruby : https://flaviocopes.com/ruby-headers-api-call/