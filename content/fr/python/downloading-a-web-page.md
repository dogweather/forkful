---
title:                "Télécharger une page web"
html_title:           "Python: Télécharger une page web"
simple_title:         "Télécharger une page web"
programming_language: "Python"
category:             "Python"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/python/downloading-a-web-page.md"
---

{{< edit_this_page >}}

## Pourquoi

Vous êtes-vous déjà demandé comment fonctionne le téléchargement d'une page web? Ou peut-être que vous avez besoin de télécharger du contenu pour un projet de scraping de données? Dans cet article, nous allons explorer comment télécharger une page web en utilisant Python.

## Comment faire

Pour télécharger une page web en utilisant Python, nous allons utiliser le module `requests`. Si vous n'avez pas encore installé ce module, vous pouvez le faire en utilisant la commande suivante dans votre terminal:

```
pip install requests
```

Ensuite, importez le module dans votre code en utilisant la commande `import`:

```python
import requests
```

Ensuite, nous allons utiliser la méthode `get()` pour récupérer le contenu de la page web avec l'URL spécifiée. Par exemple, si nous voulons télécharger la page d'accueil de Google, nous pouvons utiliser ce code:

```python
response = requests.get("https://www.google.com")
```

Vous pouvez également spécifier des paramètres supplémentaires dans la méthode `get()` tels que des en-têtes ou des données pour les demandes POST. Pour en savoir plus sur les différentes options disponibles avec le module `requests`, vous pouvez consulter sa documentation officielle.

Ensuite, nous pouvons vérifier le statut de la réponse à l'aide de la propriété `status_code`. Si la requête s'est bien déroulée, le code sera égal à `200`:

```python
print(response.status_code) # Output: 200
```

Enfin, pour obtenir le contenu HTML de la page web, nous pouvons utiliser la propriété `text` de l'objet `response`:

```python
html = response.text
```

## Deep Dive

Le module `requests` est largement utilisé pour le téléchargement de pages web en Python en raison de sa simplicité d'utilisation et de ses nombreuses fonctionnalités. Il offre également des options telles que la gestion des cookies, l'authentification et la vérification SSL.

N'oubliez pas que lors du téléchargement de pages web, vous devez toujours vérifier le statut de la réponse afin de vous assurer que la requête s'est bien déroulée. Si le statut est différent de `200`, cela peut être dû à une erreur ou à des restrictions d'accès à la page.

Au-delà du téléchargement de pages web, le module `requests` peut également être utilisé pour envoyer des requêtes à des API ou pour accéder à des fichiers à partir d'URL. Avec sa syntaxe simple et concise, il est un outil précieux pour tout développeur Python.

## Voir aussi

Pour en savoir plus sur le module `requests` et ses fonctionnalités, vous pouvez consulter la documentation officielle ici:

- https://requests.readthedocs.io/en/master/

Vous pouvez également consulter ces ressources pour en savoir plus sur le téléchargement de pages web en Python:

- https://stackabuse.com/download-files-with-python/
- https://www.geeksforgeeks.org/downloading-files-web-using-python/