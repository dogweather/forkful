---
title:                "Go: Telechargement d'une page web"
simple_title:         "Telechargement d'une page web"
programming_language: "Go"
category:             "Go"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/go/downloading-a-web-page.md"
---

{{< edit_this_page >}}

## Pourquoi

Télécharger une page web est une tâche courante pour de nombreux développeurs, que ce soit pour extraire des données, créer des bots ou simplement pour tester une application. Dans cet article, nous allons explorer comment le faire en utilisant le langage de programmation Go.

## Comment faire

Pour télécharger une page web en utilisant Go, nous allons utiliser le paquet "net/http", qui offre des fonctionnalités pour gérer les requêtes et les réponses HTTP. Voici un exemple de code pour télécharger une page web et afficher le contenu :

```
package main

import (
    "fmt"
    "net/http"
)

func main() {
    // Définir l'URL de la page à télécharger
    url := "https://www.example.com"

    // Utiliser la fonction Get du paquet "net/http" pour envoyer une requête HTTP GET à l'URL
    response, err := http.Get(url)

    // Gérer les erreurs potentielles
    if err != nil {
        fmt.Println(err)
        return
    }

    // Lire le contenu de la réponse et le stocker dans une variable
    body, err := ioutil.ReadAll(response.Body)

    // Gérer les erreurs potentielles
    if err != nil {
        fmt.Println(err)
        return
    }

    // Imprimer le contenu de la réponse
    fmt.Println(string(body))
}
```

En exécutant ce code, vous devriez voir le code HTML de la page téléchargée s'afficher dans la sortie de votre terminal.

## Profondeur de la plongée

Maintenant que nous avons vu comment télécharger une page web en utilisant Go, examinons de plus près les différentes étapes de ce processus.

- Tout d'abord, nous définissons l'URL de la page à télécharger.
- Nous utilisons ensuite la fonction Get du paquet "net/http" pour envoyer une requête HTTP GET à cette URL.
- Si la requête est réussie, nous lisons le contenu de la réponse et le stockons dans une variable.
- Enfin, nous imprimons le contenu de la réponse. Dans cet exemple, nous avons simplement utilisé la fonction "fmt.Println" pour l'afficher, mais vous pouvez l'utiliser à votre guise dans votre propre code.

Il est également important de noter que la fonction "Get" renvoie un objet "Response", qui contient des informations sur la réponse de la requête, telles que le code de statut, les en-têtes et le corps de la réponse.

## Voir aussi

Maintenant que nous savons comment télécharger une page web en utilisant Go, voici quelques autres ressources utiles pour vous aider à approfondir vos connaissances :

- [Documentation officielle sur le paquet "net/http"](https://golang.org/pkg/net/http/)
- [Tutoriel vidéo sur le téléchargement de pages web en utilisant Go](https://www.youtube.com/watch?v=a964dIqkdir)
- [Article sur l'utilisation de Go pour créer un crawler web](https://medium.com/@krishdevdb/scrape-a-web-page-with-golang-5d95a2c4d013)