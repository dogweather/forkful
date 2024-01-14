---
title:                "Python: Envoi d'une demande http"
simple_title:         "Envoi d'une demande http"
programming_language: "Python"
category:             "Python"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/python/sending-an-http-request.md"
---

{{< edit_this_page >}}

## Pourquoi
L'envoi de requêtes HTTP est une tâche courante dans le développement web moderne. Cela permet d'interagir avec des serveurs distants pour récupérer des données ou effectuer des actions. Apprenez comment envoyer des requêtes HTTP avec Python et utilisez cette compétence pour améliorer vos projets web.

## Comment Faire
Pour envoyer une requête HTTP en Python, nous allons utiliser le module requests. Assurez-vous de l'installer en utilisant la commande `pip install requests` avant de commencer. Ensuite, importez le module et spécifiez l'URL de la requête que vous souhaitez envoyer :

```python
import requests

url = "https://www.example.com" # remplacez par votre URL
```

Pour envoyer une simple requête GET, utilisez la méthode `get()` de requests et stockez la réponse dans une variable :

```python
response = requests.get(url)
```

Vous pouvez maintenant accéder au contenu de la réponse à l'aide de la méthode `text` :

```python
print(response.text)
```

Voici un exemple complet qui affiche le contenu de la page d'accueil de Google :

```python
import requests

url = "https://www.google.com"
response = requests.get(url)
print(response.text)
```

Output :

```
<!doctype html> ...
```

Vous pouvez également spécifier des paramètres supplémentaires dans votre requête en utilisant le paramètre `params` :

```python
params = {"search": "python", "limit": 100}

response = requests.get(url, params=params)
```

Pour envoyer une requête POST avec des données, utilisez le paramètre `data` :

```python
data = {"username": "John", "password": "123456"}

response = requests.post(url, data=data)
```

Pour spécifier des en-têtes personnalisés dans votre requête, utilisez le paramètre `headers` :

```python
headers = {"User-Agent": "Mozilla/5.0 (Windows NT 10.0; Win64; x64; rv:68.0) Gecko/20100101 Firefox/68.0"}

response = requests.get(url, headers=headers)
```

Pour plus d'informations et d'exemples sur l'envoi de requêtes HTTP avec Python, consultez la documentation officielle de requests : https://requests.readthedocs.io/en/master/.

## Plongée Profonde
Pour comprendre en profondeur le fonctionnement des requêtes HTTP, il est important de connaître les différents types de méthodes de requête et leurs fonctions. Les deux méthodes les plus couramment utilisées sont GET et POST.

La méthode GET est utilisée pour demander des données au serveur, tandis que la méthode POST est utilisée pour envoyer des données vers le serveur. Les autres méthodes couramment utilisées sont PUT, DELETE et PATCH, qui ont chacune un but spécifique dans la communication avec un serveur.

Il est également important de comprendre les codes de statut HTTP, qui informent sur le résultat de la requête. Par exemple, le code 200 signifie que la requête a été correctement exécutée, tandis que le code 404 signifie que la ressource demandée n'a pas été trouvée sur le serveur.

## Voir Aussi
* Tutoriel complet :https://realpython.com/python-requests/
* Documentation officielle de requests : https://requests.readthedocs.io/en/master/
* Cheatsheet de méthodes de requête HTTP : https://developer.mozilla.org/en-US/docs/Web/HTTP/Methods
* Liste des codes de statut HTTP : https://en.wikipedia.org/wiki/List_of_HTTP_status_codes