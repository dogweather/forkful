---
title:                "Envoi d'une requête HTTP avec authentification de base"
aliases:
- fr/bash/sending-an-http-request-with-basic-authentication.md
date:                  2024-01-20T18:01:00.488842-07:00
model:                 gpt-4-1106-preview
simple_title:         "Envoi d'une requête HTTP avec authentification de base"

tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/bash/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## Quoi & Pourquoi ?
L'envoi d'une requête HTTP avec authentification de base c'est transmettre des identifiants via une requête pour accéder à des ressources protégées. Les programmeurs utilisent cette méthode pour interagir de manière sécurisée avec des API ou des services web qui exigent une vérification d'identité.

## Comment faire :
```Bash
# Utiliser cURL pour envoyer une requête avec authentification de base
user="monUtilisateur"
password="monMotDePasse"
url="https://monapi.exemple.com/donnees"

# La requête
response=$(curl --user $user:$password --request GET $url)

# Afficher le résultat
echo $response
```
Sortie échantillon:
```
{"status":"success","data":"Voici vos données privées!"}
```

## Plongée en profondeur
Historiquement, l'authentification de base est une des méthodes les plus anciennes pour sécuriser des accès web. Simple à mettre en place, elle combine l'identifiant et le mot de passe (séparés par un deux-points), encode le tout en Base64, puis le transmet via l'entête 'Authorization' d'une requête HTTP.

Il existe des alternatives plus sûres, telles que l'authentification Digest, l'authentification par formulaire, OAuth, ou même l'utilisation de tokens JWT (JSON Web Tokens). L'authentification de base, bien que moins sécurisée car les credentials pourraient être interceptés en clair si la connexion n'est pas chiffrée (HTTPS), reste utilisée pour sa simplicité d'implémentation et de test lors du développement.

Concernant l'implémentation, avec Bash et cURL, le flag `--user` gère l'encodage en Base64 et l'ajout de l'entête automatiquement. Il est fortement recommandé d'utiliser HTTPS au lieu de HTTP pour sécuriser la transmission des données d'authentification.

## Voir également
- [Authentification HTTP sur MDN](https://developer.mozilla.org/fr/docs/Web/HTTP/Authentication)
- [Sécuriser une API avec OAuth](https://oauth.net/2/)
- [cURL Documentation](https://curl.se/docs/)
