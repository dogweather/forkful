---
date: 2024-01-20 17:44:42.311365-07:00
description: 'How to: (Comment faire : ) Voici un exemple simple avec `requests` .'
lastmod: '2024-04-05T22:38:57.864438-06:00'
model: gpt-4-1106-preview
summary: '(Comment faire : ) Voici un exemple simple avec `requests` .'
title: "T\xE9l\xE9chargement d'une page web"
weight: 42
---

## How to: (Comment faire : )
Voici un exemple simple avec `requests` :

```Python
import requests

# Demande de la page
reponse = requests.get('https://www.example.com')

# Afficher le contenu de la page
print(reponse.text)
```

Sortie attendue (partial sample output) :

```
<!doctype html>
<html>
<head>
    <title>Example Domain</title>
...
```

## Deep Dive (Plongée en profondeur)
Historiquement, télécharger une page web pouvait être complexe avec des sockets bas niveau et des protocoles HTTP manuels. Maintenant, `requests` facilite énormément la tâche. Alternativement, `urllib` est une option intégrée. Niveau profondeur, `requests` gère automatiquement la plupart des détails d'implémentation comme les cookies ou les en-têtes HTTP.

## See Also (Voir aussi)
- Documentation de `requests` : https://docs.python-requests.org/en/latest/
- `urllib` pour une approche sans dépendances tierces : https://docs.python.org/3/library/urllib.html
- Tutoriel HTTP : https://developer.mozilla.org/en-US/docs/Web/HTTP/Overview
