---
title:                "Ruby: Envoyer une demande http avec authentification de base"
simple_title:         "Envoyer une demande http avec authentification de base"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/ruby/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

Pourquoi: Envoyer une requête HTTP avec une authentification de base peut être nécessaire lors de la communication avec des services en ligne sécurisés, tels que les API ou les serveurs Web.

Comment faire: Pour envoyer une requête HTTP avec une authentification de base en utilisant Ruby, vous pouvez utiliser la bibliothèque standard Net::HTTP. Tout d'abord, vous devez exiger cette bibliothèque comme ceci:

```ruby
require 'net/http'
```

Ensuite, vous devez créer une instance de la classe Net::HTTP en précisant l'URL et le port du serveur auquel vous souhaitez envoyer la requête. Par exemple, si vous souhaitez envoyer une requête à `https://example.com`, vous pouvez faire comme ceci:

```ruby
http = Net::HTTP.new('example.com', 443)
```

Notez que 443 est le port par défaut pour les connexions HTTPS. Ensuite, vous devez définir l'authentification de base en ajoutant les informations d'identification dans les en-têtes de la requête. Voici un exemple de code qui vous montre comment le faire:

```ruby
req = Net::HTTP::Get.new('https://example.com/api/data')
req.basic_auth('username', 'password')
```

Enfin, vous pouvez envoyer la requête et récupérer la réponse en utilisant la méthode `#request` de l'instance Net::HTTP que vous avez créée précédemment:

```ruby
response = http.request(req)
puts response.body
```

Cela affichera le résultat de votre requête à l'écran. Si l'authentification a réussi, vous recevrez la réponse attendue. Sinon, vous obtiendrez une erreur d'authentification.

Plongée en profondeur: Envoyer une requête HTTP avec une authentification de base nécessite de préciser les informations d'identification dans les en-têtes de la requête, ainsi que de s'assurer que la connexion est sécurisée en utilisant HTTPS. Il est important de noter que les informations d'identification sont envoyées en clair, il est donc essentiel de s'assurer que la connexion est bien sécurisée.

Voir aussi: 

- [Documentation officielle Ruby pour la classe Net::HTTP](https://ruby-doc.org/stdlib-2.7.2/libdoc/net/http/rdoc/Net/HTTP.html)
- [Exemple de code pour envoyer une requête HTTP avec une authentification de base en utilisant Ruby](https://www.rubyguides.com/2018/08/ruby-http-request/)
- [Article sur l'importance de sécuriser les communications sur internet](https://www.lejdd.fr/Pourquoi-il-est-important-de-securiser-les-communications-sur-internet)