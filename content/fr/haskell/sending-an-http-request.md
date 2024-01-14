---
title:                "Haskell: Envoi d'une requête http"
simple_title:         "Envoi d'une requête http"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/haskell/sending-an-http-request.md"
---

{{< edit_this_page >}}

# Pourquoi

Les requêtes HTTP jouent un rôle crucial dans le développement d'applications Web en Haskell. Comprendre comment les utiliser efficacement vous permettra de communiquer avec des serveurs externes, d'accéder à des API et d'obtenir des données en temps réel. Dans cet article, nous allons explorer en détail les différentes façons d'envoyer une requête HTTP en Haskell.

# Comment faire

Il existe plusieurs façons d'effectuer une requête HTTP en Haskell, mais nous allons nous concentrer sur le module `Network.HTTP.Simple`. Tout d'abord, nous devons importer le module dans notre script :

```
import Network.HTTP.Simple
```

Ensuite, nous devons créer une requête en utilisant la fonction `parseRequest` et en spécifiant l'URL de notre requête :

```
request <- parseRequest "https://exemple.com"
```

Nous pouvons également spécifier une méthode différente de `GET`, par exemple `POST` :

```
request <- setRequestMethod "POST" <$> parseRequest "https://exemple.com"
```

Ensuite, nous pouvons ajouter des paramètres à notre requête en utilisant la fonction `setRequestQueryString`. Par exemple, si nous voulons ajouter un paramètre "api_key" avec la valeur "12345", nous pouvons le faire ainsi :

```
request <- setRequestQueryString [("api_key", Just "12345")] <$> parseRequest "https://exemple.com"
```

Enfin, pour envoyer notre requête, nous pouvons utiliser la fonction `httpLBS` qui renvoie une réponse de type `Response ByteString` :

```
response <- httpLBS request
```

Nous pouvons alors accéder aux données de la réponse comme son code de statut ou son corps à l'aide des fonctions `getResponseStatus` et `getResponseBody`.

# Plongée en profondeur

Le module `Network.HTTP.Simple` fournit également d'autres fonctions utiles pour gérer les requêtes HTTP. Par exemple, la fonction `httpJSON` permet d'envoyer une requête et de récupérer automatiquement les données sous forme de JSON, en utilisant le type de données `FromJSON` de la bibliothèque `aeson`.

Il existe également d'autres modules tels que `Network.HTTP.Client` et `Network.HTTP.Conduit` qui peuvent être utilisés pour une gestion plus avancée des requêtes, notamment pour gérer les connexions persistantes ou les requêtes asynchrones.

Enfin, il est important de noter que ces fonctionnalités peuvent également être utilisées pour effectuer des requêtes HTTPS sécurisées en utilisant le module `Network.HTTP.Client.TLS`.

# Voir aussi

- Documentation du module `Network.HTTP.Simple` : https://hackage.haskell.org/package/http-client/docs/Network-HTTP-Simple.html
- Tutoriel sur les requêtes HTTP en Haskell : https://www.schoolofhaskell.com/school/to-infinity-and-beyond/pick-of-the-week/Simple%20HTTP%20Client