---
title:                "Téléchargement d'une page web"
date:                  2024-01-20T17:44:00.759347-07:00
model:                 gpt-4-1106-preview
simple_title:         "Téléchargement d'une page web"

category:             "Go"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/go/downloading-a-web-page.md"
---

{{< edit_this_page >}}

## What & Why?
Télécharger une page web, c'est récupérer son contenu via Internet. Les programmeurs font cela pour analyser des données, tester des services en ligne, ou automatiser des interactions.

## How to:
La librairie standard de Go est superbe pour ce boulot. Voici un exemple simple:

```Go
package main

import (
    "fmt"
    "io"
    "net/http"
    "os"
)

func main() {
    response, err := http.Get("http://example.com")
    if err != nil {
        fmt.Println(err)
        os.Exit(1)
    }
    defer response.Body.Close()

    if response.StatusCode == http.StatusOK {
        _, err := io.Copy(os.Stdout, response.Body)
        if err != nil {
            fmt.Println(err)
        }
    } else {
        fmt.Println("Erreur lors du téléchargement:", response.Status)
    }
}
```

Sortie attendue (les premières lignes de http://example.com):

```plaintext
<!doctype html>
<html>
<head>
    <title>Example Domain</title>
    ...
</html>
```

## Deep Dive
Avant Go, on utilisait des outils comme `curl` ou des libraries telles que `libcurl` pour différentes langues de programmation. Go simplifie le processus avec `http.Get`, qui gère tous les détails sous-jacents des requêtes HTTP.

Il existe d'autres moyens de télécharger du contenu web en Go, par exemple, les librairies tierces comme `Colly` pour le scraping web plus avancé.

Au niveau de l'implémentation, la méthode `http.Get` utilise le `Client` par défaut de la librairie `http`, qui convient pour des cas d'usage simples. Pour des besoins plus complexes, créez une instance de `Client` où vous pourrez personnaliser les timeouts, les headers, et d'autres paramètres.

## See Also
- Documentation officielle de Go net/http: https://golang.org/pkg/net/http/
- Colly, un framework Go pour le scraping web: http://go-colly.org/
- Apprentissage du http en Go : https://golang.org/doc/articles/wiki/
