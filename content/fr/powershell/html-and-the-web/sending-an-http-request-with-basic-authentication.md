---
date: 2024-01-20 18:02:35.101344-07:00
description: "Envoyer une requ\xEAte HTTP avec une authentification de base, c'est\
  \ communiquer de mani\xE8re s\xE9curis\xE9e avec un serveur web en fournissant nom\
  \ d'utilisateur et\u2026"
lastmod: '2024-03-13T22:44:58.047209-06:00'
model: gpt-4-1106-preview
summary: "Envoyer une requ\xEAte HTTP avec une authentification de base, c'est communiquer\
  \ de mani\xE8re s\xE9curis\xE9e avec un serveur web en fournissant nom d'utilisateur\
  \ et mot de passe."
title: "Envoi d'une requ\xEAte HTTP avec authentification de base"
weight: 45
---

## What & Why?
Envoyer une requête HTTP avec une authentification de base, c'est communiquer de manière sécurisée avec un serveur web en fournissant nom d'utilisateur et mot de passe. Les programmeurs le font pour accéder à des ressources protégées ou interagir avec des API qui requièrent une identification.

## How to:
Pour envoyer une requête HTTP avec authentification de base en PowerShell, suivez ces étapes :

```PowerShell
# Encodez vos identifiants
$credPair = "username:password"
$encodedCreds = [System.Convert]::ToBase64String([System.Text.Encoding]::ASCII.GetBytes($credPair))

# Préparez l'en-tête d'autorisation
$headers = @{
    Authorization = "Basic $encodedCreds"
}

# Envoyez la requête
$response = Invoke-RestMethod -Uri 'https://your-api-endpoint.com/resource' -Method Get -Headers $headers

# Affichez la réponse (exemple d'objet retourné)
$response
```

Exemple de résultat :

```PowerShell
status       : 200
statusText   : OK
data         : { ... } 
```

## Deep Dive
Historiquement, l’authentification de base est une des méthodes les plus simples pour sécuriser l’accès à des ressources HTTP. Cependant, elle n’est plus considérée comme très sûre alone, surtout si les données ne sont pas envoyées via HTTPS. L'encodage Base64 des identifiants peut être facilement décodé si intercepté.

Des alternatives plus sécurisées comme OAuth 2.0 ou les clés API sont souvent préférées. Pourtant, l'authentification de base peut être utile pour des tests rapides ou des interactions avec des serveurs internes où le niveau de sécurité requis n'est pas élevé.

L'implémentation en PowerShell, surtout dans sa dernière version, utilise `Invoke-RestMethod`. Cette cmdlet facilite la création de requêtes Web complexes et la gestion des réponses. Les headers personnalisés, comme l'en-tête d’authentification dans notre exemple, sont ajoutés au tableau `$headers` qui est passé à l'appel de la cmdlet.

## See Also
- Documentation officielle de PowerShell sur `Invoke-RestMethod`: [invoke-restmethod](https://docs.microsoft.com/en-us/powershell/module/microsoft.powershell.utility/invoke-restmethod)
- Une vue d'ensemble de l'authentification HTTP de base: [http-authentication-basic-scheme](https://developer.mozilla.org/en-US/docs/Web/HTTP/Authentication#basic_authentication_scheme)
- Informations sur des méthodes d'authentification plus sécurisées: [oauth2](https://oauth.net/2/)
