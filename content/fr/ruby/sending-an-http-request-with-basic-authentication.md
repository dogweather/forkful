---
title:                "Envoyer une requête http avec une authentification de base"
html_title:           "Ruby: Envoyer une requête http avec une authentification de base"
simple_title:         "Envoyer une requête http avec une authentification de base"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/ruby/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

Qu'est-ce que c'est & pourquoi le faire?
 
L'envoi d'une requête HTTP avec une authentification de base est un moyen pour les programmeurs d'accéder à des ressources protégées sur un serveur Web. Cela implique de fournir un nom d'utilisateur et un mot de passe pour vérifier l'identité et autoriser l'accès à ces ressources.

Comment faire:

```ruby
require 'net/http'

url = URI.parse('http://example.com/protected/resource')
# Remplacez "username" et "password" par vos propres identifiants
req = Net::HTTP::Get.new(url.request_uri)
req.basic_auth 'username', 'password'

res = Net::HTTP.start(url.host, url.port) {|http|
  http.request(req)
}

puts res.body
```

Sortie:

Si les identifiants sont corrects, cela renverra le contenu de la ressource protégée. Sinon, cela renverra un code d'erreur.

Deep Dive:
 
L'authentification de base a été introduite dans les spécifications HTTP en 1999. Elle est considérée comme une méthode d'authentification de base car les identifiants sont envoyés en texte clair et n'offrent donc qu'une sécurité minimale. Il existe des alternatives plus sécurisées, telles que l'authentification Digest, mais l'authentification de base est encore très utilisée aujourd'hui en raison de sa simplicité. 

Pour implémenter une authentification de base, les serveurs Web peuvent utiliser le header `Authorization` avec la valeur `Basic` suivie d'une chaîne encodée en base64 de la forme "username:password". Côté client, les bibliothèques de requête HTTP telles que `net/http` prennent en charge l'ajout de ces informations d'authentification dans la requête.

Voir aussi:

Pour en savoir plus sur les différentes méthodes d'authentification HTTP et leurs avantages et inconvénients, consultez ces ressources:

- https://developer.mozilla.org/fr/docs/Web/HTTP/Authentication
- https://www.rfc-editor.org/info/rfc2068 (Spécification originale de l'authentification de base)
- https://www.rfc-editor.org/info/rfc7617 (Spécification de la mise à jour de l'authentification de base)