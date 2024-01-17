---
title:                "Le téléchargement d'une page web"
html_title:           "Go: Le téléchargement d'une page web"
simple_title:         "Le téléchargement d'une page web"
programming_language: "Go"
category:             "Go"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/go/downloading-a-web-page.md"
---

{{< edit_this_page >}}

## Qu'est-ce que c'est et pourquoi?
Télécharger une page web consiste à récupérer son contenu sur Internet et à l'afficher sur votre navigateur. Les programmeurs le font pour accéder à des données ou pour automatiser des tâches telles que la validation de formulaire ou le scrapping de données.

## Comment faire:
Pour télécharger une page web en utilisant Go, utilisez la fonction `Get()` du package `http` en spécifiant l'URL de la page. Par exemple:

```Go
resp, err := http.Get("https://www.example.com")
```

Cela renverra un objet `Response` qui contient le contenu de la page ainsi que des informations telles que le code de statut et les en-têtes. Pour afficher le contenu de la page, vous pouvez utiliser `resp.Body` avec la méthode `Read()` du package `io`.

```Go
body, err := io.ReadAll(resp.Body)
fmt.Println(string(body))
```

## Plongée profonde:
Télécharger des pages web est une tâche courante pour les programmeurs, en particulier pour les développeurs web. D'autres alternatives telles que cURL peuvent également être utilisées pour télécharger des pages web en utilisant des lignes de commande. De plus, la méthode `Get()` utilise une requête GET par défaut, mais vous pouvez également spécifier d'autres méthodes HTTP telles que POST ou PUT.

## Voir aussi:
Pour plus d'informations sur la récupération des pages web en utilisant Go, vous pouvez consulter la documentation officielle: https://golang.org/pkg/net/http/#Get. Vous pouvez également découvrir d'autres fonctionnalités intéressantes du package `http` telles que la gestion des cookies et l'utilisation de clients personnalisés.