---
title:                "Python: Envoi d'une requête http avec authentification de base"
simple_title:         "Envoi d'une requête http avec authentification de base"
programming_language: "Python"
category:             "Python"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/python/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## Pourquoi

Il est important de comprendre comment envoyer une demande HTTP avec une authentification basique en Python car cela permet de sécuriser les informations sensibles échangées entre un client et un serveur. Cela peut également être utilisé pour restreindre l'accès à certaines ressources en ligne.

## Comment faire

Pour envoyer une demande HTTP avec une authentification basique en Python, il faut suivre les étapes suivantes :

1. Importer le module `requests` en utilisant la commande ```Python
import requests
```
2. Définir les informations d'identification de base (utilisateur et mot de passe) en utilisant la fonction `auth = (utilisateur, mot_de_passe)`.
3. Définir l'URL de la ressource à laquelle on souhaite accéder.
4. Utiliser la méthode `get()` pour envoyer la demande et attribuer le résultat à une variable.
5. Vérifier le code de statut de la réponse pour s'assurer que la demande a été effectuée avec succès.
6. Utiliser la méthode `text` pour afficher le contenu de la réponse.

Voici un exemple de code complet pour envoyer une demande GET avec authentification basique :

```Python
import requests

auth = ('utilisateur', 'mot_de_passe')
url = 'https://exemple.com/restricted_resource'
response = requests.get(url, auth=auth)

if response.status_code == 200:
    print(response.text)
else:
    print('Erreur : ', response.status_code)
```

Voici un exemple de sortie pour une demande réussie :

```
Bienvenue sur la ressource restreinte !
```

Voici un exemple de sortie pour une demande échouée :

```
Erreur : 401
```

## Plongée en profondeur

En utilisant la méthode `get()`, il est également possible de spécifier des paramètres supplémentaires tels que des en-têtes et des données à inclure dans la demande. De plus, la méthode `auth` peut être utilisée pour spécifier différentes méthodes d'authentification telles que l'authentification par formulaire ou l'authentification OAuth.

Il est également important de noter que l'utilisation d'une authentification basique n'est pas suffisante pour sécuriser complètement une application. D'autres mesures de sécurité doivent être prises en compte pour protéger les informations sensibles et éviter les attaques.

## Voir aussi

- [Documentation sur les demandes Python](https://docs.python-requests.org/en/master/)
- [Guide d'authentification de base IETF](https://tools.ietf.org/html/rfc7617)
- [Exemple de demande HTTP avec authentification basique en Python](https://gist.github.com/anonymous/d127c51b5b6341da3f8dfacad0ac123f)