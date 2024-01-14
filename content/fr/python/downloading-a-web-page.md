---
title:                "Python: Téléchargement d'une page web"
simple_title:         "Téléchargement d'une page web"
programming_language: "Python"
category:             "Python"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/python/downloading-a-web-page.md"
---

{{< edit_this_page >}}

## Pourquoi

Il y a de nombreuses raisons pour lesquelles vous pourriez vouloir télécharger une page web à partir de Python. Peut-être que vous voulez extraire des données à des fins de traitement, ou peut-être que vous voulez simplement sauvegarder une page pour la lire hors ligne. Dans tous les cas, le téléchargement de pages web à partir de Python peut être une compétence utile à avoir.

## Comment faire

Pour télécharger une page web à partir de Python, vous aurez besoin d'utiliser le module "requests". Tout d'abord, importez le module en utilisant la commande suivante :

```Python
import requests
```

Ensuite, utilisez la fonction "get" pour récupérer le contenu de la page web en spécifiant l'URL comme argument. Par exemple :

```Python
r = requests.get("www.examplewebsite.com")
```

Maintenant, vous pouvez accéder au contenu de la page web en utilisant la variable "r". Vous pouvez vérifier le code d'état de la requête en utilisant la propriété "status_code" :

```Python
print(r.status_code)
```

Si le code d'état est 200, cela signifie que la requête s'est déroulée avec succès et vous pouvez accéder au contenu de la page en utilisant la propriété "content" :

```Python
print(r.content)
```

## Plongée en profondeur

Il y a quelques choses à prendre en compte lors du téléchargement de pages web à partir de Python. Tout d'abord, vous voudrez peut-être spécifier des en-têtes HTTP pour que le serveur web sache quelle page vous voulez télécharger. Vous pouvez le faire en utilisant l'argument "headers" dans la fonction "get".

Deuxièmement, il est important de noter que certaines pages web peuvent nécessiter une authentification avant de pouvoir être téléchargées. Pour ce faire, utilisez l'argument "auth" dans la fonction "get" en spécifiant vos informations d'identification.

Enfin, il est important de noter que certaines pages web peuvent éventuellement être protégées par un pare-feu. Dans ce cas, il peut être nécessaire d'utiliser un proxy pour le téléchargement.

## Voir aussi

- Documentation officielle de requests : https://requests.readthedocs.io/en/master/
- Tutoriel "Téléchargement de pages web avec Python" : https://realpython.com/python-web-scraping-practical-introduction/