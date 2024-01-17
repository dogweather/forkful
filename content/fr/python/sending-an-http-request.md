---
title:                "Envoi d'une requête http"
html_title:           "Python: Envoi d'une requête http"
simple_title:         "Envoi d'une requête http"
programming_language: "Python"
category:             "Python"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/python/sending-an-http-request.md"
---

{{< edit_this_page >}}

## Qu'est-ce que c'est et pourquoi les programmeurs le font?

Envoyer une requête HTTP est un moyen pour les programmeurs d'envoyer des instructions à un serveur distant et d'obtenir des réponses en retour. Cela permet aux applications de communiquer entre elles et d'accéder à des ressources en ligne.

## Comment faire:

Voici un exemple de code Python pour envoyer une requête GET à un serveur et afficher le contenu de la réponse:

```Python
import requests
r = requets.get("https://www.example.com")
print(r.text)
```

Le code ci-dessus utilise la bibliothèque "requests" pour envoyer une requête GET à l'URL "https://www.example.com" et stocke la réponse dans une variable appelée "r". Ensuite, la réponse est affichée en utilisant la méthode "text" de la variable "r".

Voici un autre exemple utilisant la méthode "post" pour envoyer des données à un serveur et afficher le code d'état de la réponse:

```Python
import requests
data = {"key": "value"}
r = requests.post("https://www.example.com", data=data)
print(r.status_code)
```

## Profondeur de plongée:

Les requêtes HTTP sont largement utilisées dans le développement web pour permettre aux navigateurs d'obtenir du contenu à partir de serveurs distants. Elles ont été introduites en 1996 dans la version 1.0 du protocole HTTP et ont depuis évolué pour inclure plusieurs méthodes telles que GET, POST, PUT, DELETE, etc.

Bien qu'il existe d'autres méthodes de communication entre serveurs et applications, telles que les sockets et les websockets, les requêtes HTTP restent une méthode populaire et largement prise en charge.

Pour implémenter des requêtes HTTP en Python, il existe d'autres bibliothèques telles que "urllib" et "http.client", mais "requests" est généralement préférée pour sa simplicité et ses fonctionnalités supplémentaires telles que la gestion transparente des cookies et des en-têtes.

## Voir aussi:

Pour en savoir plus sur les requêtes HTTP et leur utilisation en Python, voici quelques ressources utiles:

- La documentation officielle de la bibliothèque "requests": https://requests.readthedocs.io/en/master/
- Un tutoriel sur les requêtes HTTP en Python: https://realpython.com/python-requests/
- Un guide sur le protocole HTTP: https://developer.mozilla.org/fr/docs/Web/HTTP/Overview