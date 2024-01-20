---
title:                "Envoyer une requête http"
html_title:           "Bash: Envoyer une requête http"
simple_title:         "Envoyer une requête http"
programming_language: "Python"
category:             "Python"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/python/sending-an-http-request.md"
---

{{< edit_this_page >}}

## Qu'est-ce que et Pourquoi?

Envoyer une requête HTTP, c'est comme envoyer une lettre à un serveur Web pour demander certaines informations. Les programmeurs le font pour interagir avec les API, récupérer des informations des sites Web et même envoyer des données sur Internet.

## Comment faire:

En Python, nous utilisons le module `requests` pour envoyer des requêtes HTTP :

```Python
import requests

reponse = requests.get('https://www.example.com')
print(reponse.text)
```

Dans cet exemple, nous demandons au serveur de 'https://www.example.com' de nous envoyer son contenu. Si tout va bien, les données de la page Web s'afficheront.

## Plongeons plus profondément 

Historiquement, nous utilisions httplib ou urllib en Python pour les requêtes HTTP, mais elles sont trop verboses et compliquées. `Requests` est devenu le standard de facto en raison de sa simplicité.

Un autre module courant pour traiter les requêtes HTTP en Python est `http.client` (anciennement httplib), qui fait partie de la bibliothèque standard. 

En termes d'implémentation, lorsqu'une requête est envoyée, elle passe par plusieurs couches (SSL, TCP, IP) avant d'arriver au serveur. Le serveur traite la requête et renvoie une réponse, qui suit le même chemin de retour jusqu'à votre application.

Noter:
- Une requête GET est envoyée dans l'exemple, mais d'autres types de requêtes peuvent être envoyées comme POST, DELETE, PUT, etc.
- Chaque requête HTTP a un code de statut. Un code "200" signifie que tout s'est bien passé.

## Voir aussi 

- Documentation Python requests : https://docs.python-requests.org/fr/latest/
- Explainer sur les requêtes HTTP : https://developer.mozilla.org/fr/docs/Web/HTTP/Overview
- Tutoriel sur l'interaction avec les API à l'aide de requests : https://realpython.com/python-requests/#the-get-request