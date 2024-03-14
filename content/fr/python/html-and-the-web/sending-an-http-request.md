---
date: 2024-01-20 18:00:31.642041-07:00
description: "Envoyer une requ\xEAte HTTP permet \xE0 votre programme d'obtenir ou\
  \ d'envoyer des donn\xE9es sur le web. C'est essentiel pour interagir avec des API,\
  \ des services\u2026"
lastmod: '2024-03-13T22:44:57.232209-06:00'
model: gpt-4-1106-preview
summary: "Envoyer une requ\xEAte HTTP permet \xE0 votre programme d'obtenir ou d'envoyer\
  \ des donn\xE9es sur le web. C'est essentiel pour interagir avec des API, des services\u2026"
title: "Envoi d'une requ\xEAte HTTP"
---

{{< edit_this_page >}}

## What & Why?
Envoyer une requête HTTP permet à votre programme d'obtenir ou d'envoyer des données sur le web. C'est essentiel pour interagir avec des API, des services web ou tout simplement pour récupérer du contenu en ligne.

## How to:
Utiliser Python pour envoyer des requêtes HTTP est simple avec la bibliothèque `requests`. D'abord, installez-le si ce n'est pas déjà fait :

```bash
pip install requests
```

Ensuite, un exemple basique pour récupérer le contenu d'une page web :

```Python
import requests

reponse = requests.get('https://www.example.com')
print(reponse.status_code)
print(reponse.text[:200])  # Affiche les 200 premiers caractères du contenu.
```

Pour envoyer des données en utilisant POST :

```Python
donnees = {'key': 'value'}
reponse = requests.post('https://www.example.com', data=donnees)
print(reponse.status_code)
```

Résultats typiques :

```
200
<!doctype html> ...
```

```
200
```

## Deep Dive
Historiquement, l'envoi de requêtes HTTP s'effectuait en Python avec `urllib`. `requests` est apparu comme une alternative plus simple et plus conviviale. 

Contrairement à `urllib`, `requests` simplifie le processus en gérant automatiquement les cookies, les en-têtes et les paramètres, rendant le code plus intuitif. De plus, `requests` gère les exceptions plus clairement et supporte mieux les sessions HTTP.

Par exemple, avec `requests` on peut gérer une session avec context manager, comme ceci :

```Python
with requests.Session() as session:
    session.get('https://www.example.com')  # utilise des cookies persistants si nécessaire
```

Un autre point fort de `requests` est la gestion des requêtes asynchrones via `requests-async` ou `httpx` pour des workflows encore plus efficaces.

## See Also
- Documentation de `requests`: https://requests.readthedocs.io/en/master/
- Comparaison entre `requests` et `urllib`: https://findwork.dev/blog/advanced-usage-python-requests-timeouts-retries-hooks/
- `httpx`, une bibliothèque tierce pour les requêtes HTTP asynchrones: https://www.encode.io/httpx/
