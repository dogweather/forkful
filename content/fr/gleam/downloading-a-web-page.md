---
title:                "Téléchargement d'une page web"
date:                  2024-01-20T17:43:50.876489-07:00
model:                 gpt-4-1106-preview
simple_title:         "Téléchargement d'une page web"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/gleam/downloading-a-web-page.md"
---

{{< edit_this_page >}}

## Quoi & Pourquoi ?
Télécharger une page web, c'est récupérer son contenu via le réseau. Les programmeurs le font pour analyser des données, tester des sites ou automatiser des tâches web.

## Comment faire :
```gleam
import gleam/http
import gleam/httpc

pub fn download_webpage(url: String) -> Result(String, Nil) {
  httpc.send(http.Request(method: Get, url: url))
  |> result.map(fn(response) { response.body })
}
```
Exemple de sortie :
```
Ok("<html>...</html>")
```

## Plongée en profondeur
Historiquement, le téléchargement de pages web a débuté avec des outils comme `wget` et `curl`. En Gleam, `httpc` est la bibliothèque standard pour les requêtes HTTP, mais il y a aussi d'autres options comme `gleam_cowboy` pour des cas d'usage plus spécifiques. Les détails d'implémentation de `httpc` incluent la gestion des erreurs de réseau et des codes de statut HTTP pour une manipulation robuste des résultats.

## Voir aussi
- `httpc` module docs: [https://hexdocs.pm/gleam_httpc](https://hexdocs.pm/gleam_httpc)
- Gleam website for more general information: [https://gleam.run](https://gleam.run)