---
title:                "Envoyer une requête http avec une authentification de base"
html_title:           "Python: Envoyer une requête http avec une authentification de base"
simple_title:         "Envoyer une requête http avec une authentification de base"
programming_language: "Python"
category:             "Python"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/python/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## Qu'est-ce que c'est & pourquoi ?
Envoyer une requête HTTP avec une authentification de base signifie simplement inclure un nom d'utilisateur et un mot de passe dans la requête HTTP. Les programmeurs font cela pour s'assurer que seuls les utilisateurs autorisés peuvent accéder aux ressources en ligne.

## Comment faire:
Voici un exemple de code Python pour envoyer une requête HTTP avec une authentification de base:
```
import requests # 1
url = 'https://monsite.com' # 2
username = 'monnomdutilisateur' # 3
password = 'monmotdepasse' # 4
response = requests.get(url, auth=(username, password)) # 5
print(response.status_code) # 6
```
1. Tout d'abord, nous importons la bibliothèque "requests" qui nous permettra d'effectuer des requêtes HTTP. 
2. Ensuite, nous définissons l'URL de notre site.
3. Nous attribuons le nom d'utilisateur à une variable "username".
4. Nous attribuons le mot de passe à une variable "password".
5. En utilisant la méthode "get" de la bibliothèque "requests", nous appelons l'URL en incluant les informations d'authentification dans le paramètre "auth".
6. Enfin, nous imprimons le code de statut de la réponse pour vérifier si la requête a réussi.

## Focus en profondeur:
1. L'authentification de base a été introduite pour la première fois dans le protocole HTTP en 1999 dans la spécification RFC 2617. Elle est considérée comme une méthode d'authentification de base et peu sécurisée.
2. Bien que l'authentification de base soit simple à mettre en œuvre, elle n'est pas recommandée pour les sites Web sensibles car le nom d'utilisateur et le mot de passe sont envoyés en clair à chaque requête.
3. L'authentification de base peut être implémentée en utilisant d'autres langages de programmation, tels que PHP, Java ou Ruby. Le principe reste le même : inclure les informations d'authentification dans la requête HTTP.

## Voir aussi:
- [Spécification RFC 2617](https://tools.ietf.org/html/rfc2617)
- [Documentation "Requests" en français](https://docs.python-requests.org/fr/latest/user/authentication.html#basic-authentication)