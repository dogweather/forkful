---
title:                "Envoyer une requête http avec une authentification de base"
html_title:           "PowerShell: Envoyer une requête http avec une authentification de base"
simple_title:         "Envoyer une requête http avec une authentification de base"
programming_language: "PowerShell"
category:             "PowerShell"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/powershell/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

# Réaliser une requête HTTP avec une authentification de base en PowerShell

## Quoi et Pourquoi?
Envoyer une requête HTTP avec une authentification de base consiste à inclure dans la requête un en-tête d'authentification de base contenant un nom d'utilisateur et un mot de passe encodés en base64. Les programmeurs le font pour permettre l'accès à des ressources protégées par une authentification.

## Comment faire:
Voici un exemple de code PowerShell pour réaliser une requête GET avec une authentification de base:

```PowerShell
# Importer le module d'authentification de base
Import-Module BitsTransfer

# Définir les informations d'authentification
$username = "utilisateur"
$password = "motdepasse"
$secrets = "${username}:${password}"

# Encoder les informations en base64
$encodedSecrets = [System.Convert]::ToBase64String([System.Text.Encoding]::UTF8.GetBytes($secrets))

# Créer une requête HTTP avec l'en-tête d'authentification de base
$url = "https://www.example.com"
$webRequest = [System.Net.WebRequest]::Create($url)
$webRequest.Headers.Add("Authorization", "Basic $encodedSecrets")

# Envoyer la requête et récupérer la réponse
$webResponse = $webRequest.GetResponse()
$webResponse.StatusCode
$webResponse.StatusDescription
```

La sortie devrait ressembler à ceci:

```
OK
200
OK
```

## Plongée en profondeur:
L'authentification de base a été définie dans la première version du protocole HTTP en 1996. Elle est considérée comme peu sécurisée car les informations d'identification sont envoyées en clair dans la requête. Des alternatives plus sécurisées, comme l'authentification digest, ont été développées mais l'authentification de base reste largement utilisée en raison de sa simplicité d'implémentation.

## Voir aussi:
- [Documentation officielle de l'authentification de base en HTTP](https://tools.ietf.org/html/rfc2617)
- [Comparaison entre l'authentification de base et l'authentification digest](https://developer.mozilla.org/fr/docs/Web/Security/HTTP_Authentication)