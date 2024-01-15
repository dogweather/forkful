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

## Pourquoi

Vous vous demandez peut-être pourquoi il est important d'envoyer une requête HTTP en utilisant Python. Eh bien, cela peut être utile pour interagir avec des serveurs web et obtenir des données à partir de ceux-ci.

## Comment faire

Pour envoyer une requête HTTP en utilisant Python, vous pouvez utiliser le module "requests". Tout d'abord, importez-le en utilisant la ligne de code suivante :

```Python
import requests
```

Ensuite, vous pouvez utiliser la fonction "get" pour envoyer une requête GET à un serveur et obtenir la réponse. Vous devrez également spécifier l'URL du serveur que vous souhaitez interroger. Voici un exemple de code:

```Python
response = requests.get("https://www.example.com")
```

Si la requête est réussie, vous pouvez utiliser la méthode "text" pour afficher le contenu de la réponse :

```Python
print(response.text)
```

Et voilà ! Vous avez maintenant envoyé une requête HTTP en utilisant Python et obtenu la réponse du serveur.

## Fouiller plus en profondeur

Le module "requests" propose également d'autres méthodes de requête telles que "post", "put" et "delete" pour interagir avec des serveurs web de différentes manières. Vous pouvez également spécifier des paramètres, des en-têtes ou des données à inclure dans la requête lorsque vous l'envoyez.

Il est également important de noter que le module "requests" gère automatiquement la gestion des erreurs et des redirections lors de l'envoi de requêtes HTTP.

## Voir aussi

Voici quelques liens utiles pour en savoir plus sur l'envoi de requêtes HTTP en utilisant Python :

- [Documentation officielle du module requests](https://docs.python-requests.org/en/master/)
- [Tutoriel sur les requêtes HTTP avec Python](https://realpython.com/python-requests/)
- [Exemples pratiques d'utilisation de requests en Python](https://www.thepythoncode.com/article/make-http-requests-in-python)