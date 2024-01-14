---
title:                "Gleam: Envoyer une requête http avec une authentification de base"
simple_title:         "Envoyer une requête http avec une authentification de base"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/gleam/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

La programmation en Gleam: Comment utiliser une requête HTTP avec authentification de base

# Pourquoi

Lorsque vous développez une application à l'aide de Gleam, il peut être nécessaire d'envoyer des requêtes HTTP avec authentification de base. Cela peut être utile pour communiquer avec des API externes ou pour sécuriser l'accès à certaines ressources de votre serveur.

# Comment faire

Pour envoyer une requête HTTP avec authentification de base en utilisant Gleam, vous pouvez suivre les étapes suivantes :

```gleam
import gleam/http

// Définir les informations d'authentification
let username = "utilisateur123"
let password = "motdepass"

// Créer un en-tête d'authentification de base
let auth = http.basic_auth(username, password)

// Créer une requête GET avec l'en-tête d'authentification
let request = http.get("https://monapi.com/resource", [auth])

// Envoyer la requête et stocker la réponse
let response = http.send(request)

// Afficher la réponse
gleam/core/io.print(response.body)
```

Ce code va créer une requête GET sur l'URL https://monapi.com/resource en utilisant l'authentification de base avec le nom d'utilisateur "utilisateur123" et le mot de passe "motdepass". La réponse de la requête sera stockée dans la variable "response" et l'affichera dans la console.

# Plongée en profondeur

Lorsque vous envoyez une requête HTTP avec authentification de base, vous devez vous assurer que les informations d'authentification sont transmises de manière sécurisée. Pour cela, vous pouvez utiliser le protocole HTTPS pour chiffrer la communication entre le client et le serveur.

De plus, il est important de vérifier que les informations d'authentification sont correctes avant d'envoyer la requête. Cela peut être fait en utilisant des fonctions de hachage (comme SHA-256) pour chiffrer le mot de passe avant de l'envoyer.

# Voir aussi

Pour en savoir plus sur la programmation en Gleam, vous pouvez consulter les ressources suivantes :

- [Documentation officielle de Gleam](https://gleam.run/)
- [Tutoriels de programmation en Gleam](https://dev.to/t/gleam)
- [Exemples de code en Gleam](https://github.com/gleam-lang/gleam/tree/master/examples)