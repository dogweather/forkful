---
title:                "Télécharger une page web"
html_title:           "Bash: Télécharger une page web"
simple_title:         "Télécharger une page web"
programming_language: "Go"
category:             "Go"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/go/downloading-a-web-page.md"
---

{{< edit_this_page >}}

## Quoi & Pourquoi ?

Télécharger une page Web signifie récupérer son contenu HTML. Les programmeurs le font pour analyser, tester ou utiliser ultérieurement ses données.

## Comment faire :

Voici un exemple de code pour télécharger une page Web avec Go. Le code utilise le package `net/http` pour envoyer une requête GET, puis enregistre la réponse.

```go
package main

import (
   "io/ioutil"
   "net/http"
)

func main() {
    resp, err := http.Get("http://example.com")
    if err != nil {
        panic(err)
    }
    defer resp.Body.Close()

    body, err := ioutil.ReadAll(resp.Body)
    if err != nil {
        panic(err)
    }
    ioutil.WriteFile("example.html", body, 0644)
}
```

Lorsque vous exécutez ce programme, un fichier "example.html" est créé contenant le contenu de la page Web.

## Plongée Profonde :

Historiquement, le téléchargement de pages Web était surtout utilisé pour l'exploration de données ou le "scraping".  Aujourd'hui, c'est un outil essentiel pour les tests automatiques, l'analyse des performances et bien plus encore.

Si `net/http` ne répond pas à vos besoins, vous pouvez essayer des bibliothèques telles que `GoQuery` ou `Colly`. Ces outils offrent des fonctionnalités supplémentaires, comme le parcours du DOM ou le support des sites Web dynamiques.

Quant au fonctionnement interne, lorsque vous envoyez une requête GET, le serveur Web renvoie le code HTML de la page. Ce code est ensuite sauvegardé dans un fichier sur votre disque dur.

## Voir Aussi :

Pour plus d'informations sur ce sujet, consultez les liens suivants :
- Documentation officielle Go net/http: https://golang.org/pkg/net/http/
- GoQuery pour l'analyse de HTML : https://github.com/PuerkitoBio/goquery
- Colly, un cadre de scraping en Go : http://go-colly.org/