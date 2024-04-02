---
date: 2024-01-20 18:01:00.488842-07:00
description: "L'envoi d'une requ\xEAte HTTP avec authentification de base c'est transmettre\
  \ des identifiants via une requ\xEAte pour acc\xE9der \xE0 des ressources prot\xE9\
  g\xE9es. Les\u2026"
lastmod: '2024-03-13T22:44:57.994303-06:00'
model: gpt-4-1106-preview
summary: "L'envoi d'une requ\xEAte HTTP avec authentification de base c'est transmettre\
  \ des identifiants via une requ\xEAte pour acc\xE9der \xE0 des ressources prot\xE9\
  g\xE9es. Les\u2026"
title: "Envoi d'une requ\xEAte HTTP avec authentification de base"
weight: 45
---

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
