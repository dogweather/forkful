---
title:                "Analisando HTML"
aliases:
- pt/go/parsing-html.md
date:                  2024-02-03T18:00:01.372714-07:00
model:                 gpt-4-0125-preview
simple_title:         "Analisando HTML"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/go/parsing-html.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## O Que & Por Quê?

Analisar HTML em Go envolve examinar o conteúdo de arquivos HTML para extrair dados, manipular a estrutura ou converter o HTML em outros formatos. Programadores fazem isso para scraping na web, criação de templates e mineração de dados, aproveitando os fortes recursos de concorrência do Go para o processamento eficiente de grandes volumes de páginas da web.

## Como fazer:

Para analisar HTML em Go, normalmente se usa o pacote `goquery` ou o pacote `net/html` da biblioteca padrão. Aqui está um exemplo básico usando `net/html` para extrair todos os links de uma página da web:

```go
package main

import (
    "fmt"
    "golang.org/x/net/html"
    "net/http"
)

func main() {
    // Obter o documento HTML
    res, err := http.Get("http://example.com")
    if err != nil {
        panic(err)
    }
    defer res.Body.Close()

    // Analisar o documento HTML
    doc, err := html.Parse(res.Body)
    if err != nil {
        panic(err)
    }

    // Função para percorrer recursivamente o DOM
    var f func(*html.Node)
    f = func(n *html.Node) {
        if n.Type == html.ElementNode && n.Data == "a" {
            for _, a := range n.Attr {
                if a.Key == "href" {
                    fmt.Println(a.Val)
                    break
                }
            }
        }
        for c := n.FirstChild; c != nil; c = c.NextSibling {
            f(c)
        }
    }

    // Percorrer o DOM
    f(doc)
}
```

Saída de exemplo (assumindo que `http://example.com` contém dois links):

```
http://www.iana.org/domains/example
http://www.iana.org/domains/reserved
```

Este código solicita uma página HTML, a analisa e percorre recursivamente o DOM para encontrar e imprimir atributos `href` de todas as tags `<a>`.

## Aprofundamento

O pacote `net/html` oferece os fundamentos para analisar HTML em Go, implementando diretamente os algoritmos de tokenização e construção de árvores especificados pelo padrão HTML5. Esta abordagem de baixo nível é poderosa, mas pode ser verbosa para tarefas complexas.

Em contraste, o pacote de terceiros `goquery`, inspirado pelo jQuery, oferece uma interface de nível mais alto que simplifica a manipulação e o percurso do DOM. Ele permite que os desenvolvedores escrevam códigos concisos e expressivos para tarefas como seleção de elementos, extração de atributos e manipulação de conteúdo.

No entanto, a conveniência do `goquery` vem com o custo de uma dependência adicional e um desempenho potencialmente mais lento devido à sua camada de abstração. A escolha entre `net/html` e `goquery` (ou outras bibliotecas de análise) depende dos requisitos específicos do projeto, como a necessidade de otimização de desempenho ou facilidade de uso.

Historicamente, a análise de HTML em Go evoluiu de operações básicas com strings para sofisticadas manipulações de árvores DOM, refletindo o crescente ecossistema da linguagem e a demanda da comunidade por ferramentas robustas de scraping na web e extração de dados. Apesar das capacidades nativas, a prevalência de bibliotecas de terceiros como `goquery` destaca a preferência da comunidade Go por códigos modulares e reutilizáveis. No entanto, para aplicações críticas de desempenho, programadores podem ainda favorecer o pacote `net/html` ou até recorrer a regex para tarefas simples de análise, tendo em mente os riscos inerentes e limitações da análise de HTML baseada em regex.
