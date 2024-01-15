---
title:                "Le téléchargement d'une page web"
html_title:           "Haskell: Le téléchargement d'une page web"
simple_title:         "Le téléchargement d'une page web"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/haskell/downloading-a-web-page.md"
---

{{< edit_this_page >}}

## Pourquoi

Si vous voulez apprendre à programmer en Haskell, l'une des tâches de base que vous devriez maîtriser est le téléchargement d'une page web. Cela vous permettra de récupérer de l'information à partir d'Internet et de l'utiliser dans votre programme.

## Comment faire

Pour télécharger une page web en Haskell, vous devez utiliser la bibliothèque `http-conduit`. Tout d'abord, importez-la dans votre fichier :

```Haskell
import Network.HTTP.Conduit
```

Ensuite, vous pouvez utiliser la fonction `simpleHttp` pour télécharger une page à partir de son URL :

```Haskell
simpleHttp "https://www.example.com"
```

Vous pouvez ensuite traiter l'output comme une chaîne de caractères (String) ou le convertir en un type de données tel que `ByteString`.

## Plongée en profondeur

La bibliothèque `http-conduit` utilise la gestion des exceptions pour gérer les erreurs lors du téléchargement d'une page web. Vous pouvez utiliser les fonctions `catch` et `handle` pour gérer ces exceptions.

De plus, vous pouvez également spécifier des options de requête telles que les en-têtes (headers), les cookies et les timeouts pour personnaliser votre demande de téléchargement.

## Voir aussi

- [Documentation de `http-conduit`](https://hackage.haskell.org/package/http-conduit)
- [Exemples de code en Haskell](https://wiki.haskell.org/Documentation/Examples)