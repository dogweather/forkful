---
title:                "Analisando HTML"
html_title:           "Go: Analisando HTML"
simple_title:         "Analisando HTML"
programming_language: "Go"
category:             "Go"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/go/parsing-html.md"
---

{{< edit_this_page >}}

## O que é e por que fazemos?

Parsing HTML é o processo de analisar e interpretar o código HTML de uma página da web. Isso pode ser útil para programadores que desejam extrair informações específicas de um site ou automatizar tarefas de web scraping. 

## Como fazer:

```Go
package main

import (
    "fmt"
    "net/http"

    "golang.org/x/net/html"
)

func main() {
    // Create a new HTTP client
    client := &http.Client{}

    // Make a GET request to the desired webpage
    resp, err := client.Do(http.NewRequest("GET", "https://www.example.com", nil))

    // Check for errors
    if err != nil {
        fmt.Println(err)
        return
    }

    // Parse the HTML document
    doc, err := html.Parse(resp.Body)

    // Check for errors
    if err != nil {
        fmt.Println(err)
        return
    }

    // Print the title of the webpage
    fmt.Println(doc.FirstChild.LastChild.FirstChild.FirstChild.Data)
}
```

Output:
```
Example Domain
```

## Aprofundando:

Parsing HTML é uma tarefa comum e importante na criação de aplicativos web. Existem outras linguagens de programação e ferramentas disponíveis para analisar e manipular HTML, como Python, JavaScript e bibliotecas como BeautifulSoup. No entanto, o Go oferece uma solução nativa e poderosa para essa tarefa, por meio do pacote "x/net/html". Com ele, é possível navegar pela árvore de elementos HTML e extrair dados precisos de uma página da web. 

## Veja também:

- [Documentação oficial do pacote "x/net/html"](https://pkg.go.dev/golang.org/x/net/html)
- [Exemplo de web scraper em Go](https://github.com/gocolly/colly)
- [Tutorial de parsing de HTML com Go](https://medium.com/@matteorenzi/parsing-html-in-golang-a-tutorial-e027cccc5221)