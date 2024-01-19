---
title:                "Envoyer une requête http avec une authentification de base"
html_title:           "Arduino: Envoyer une requête http avec une authentification de base"
simple_title:         "Envoyer une requête http avec une authentification de base"
programming_language: "Python"
category:             "Python"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/python/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## Qu'est-ce que c'est et Pourquoi ?
Envoyer une requête HTTP avec une authentification de base, c'est utiliser un nom d'utilisateur et un mot de passe pour accéder à une ressource. Les programmeurs le font pour protéger l'accès aux données sensibles.

## Comment faire :
Pour envoyer une requête HTTP avec une authentification de base en Python, nous pouvons utiliser la bibliothèque `requests`. Voici un exemple simple :

```Python
import requests
from requests.auth import HTTPBasicAuth

response = requests.get('https://your-url.com', auth=HTTPBasicAuth('username', 'password'))
print(response.status_code)
```
Après avoir exécuté ce code, vous verrez le code de statut HTTP imprimé. Par exemple, "200" signifie que votre demande a réussi.

## Plongée en profondeur 
L'envoi de requêtes HTTP avec une authentification de base est une pratique courante depuis les premiers jours du web. Cependant, il est important de noter que sans une connexion sécurisée HTTPS, ces références peuvent être facilement interceptées.

Une alternative à l'authentification de base est l'authentification de l'ours (Bearer Authentication), où un jeton d'accès est envoyé à la place du nom d'utilisateur et du mot de passe. Cette méthode est souvent préférée dans les applications modernes.

Un détail à noter concernant l'implémentation de l'authentification de base avec `requests` est que, par défaut, une requête sera envoyée sans authentification pour voir si elle est nécessaire. Si un code de statut `401 Unauthorized` est reçu, `requests` envoie à nouveau la requête, cette fois-ci avec authentification.

## Voir aussi 
- [Documentation officielle de Requests](https://docs.python-requests.org/en/latest/)
- [Authentification HTTP sur MDN](https://developer.mozilla.org/fr/docs/Web/HTTP/Authentication)
- [Authentification de l'ours sur OAuth](https://oauth.net/2/bearer-tokens/)