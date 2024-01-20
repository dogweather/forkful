---
title:                "Análise de HTML"
date:                  2024-01-20T15:31:44.163297-07:00
html_title:           "Bash: Análise de HTML"
simple_title:         "Análise de HTML"
programming_language: "Go"
category:             "Go"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/go/parsing-html.md"
---

{{< edit_this_page >}}

## O Que é & Por Quê?

Parsing de HTML é o processo de analisar e transformar códigos HTML em uma estrutura de dados compreensível. Programadores fazem isso para manipular, extrair informações ou interagir com páginas web de forma automática.

## Como Fazer:

```Go
package main

import (
	"fmt"
	"golang.org/x/net/html"
	"net/http"
	"strings"
)

func main() {
	resp, err := http.Get("http://example.com")
	if err != nil {
		panic(err)
	}
	defer resp.Body.Close()

	z := html.NewTokenizer(resp.Body)

	for {
		tt := z.Next()

		switch tt {
		case html.ErrorToken:
			return
		case html.StartTagToken, html.EndTagToken:
			t := z.Token()
			if t.Data == "a" {
				fmt.Println("We found a link!")
				for _, a := range t.Attr {
					if a.Key == "href" {
						fmt.Println(" - The link's URL is:", strings.TrimSpace(a.Val))
					}
				}
			}
		}
	}
}
```

Saída de exemplo:
```
We found a link!
 - The link's URL is: http://www.iana.org/domains/example
```

## Mergulho Profundo

Historicamente, parsing de HTML foi uma tarefa complicada devido à natureza flexível e às vezes mal estruturada do HTML na web. Inicialmente, as abordagens eram baseadas em expressões regulares, mas isso pode falhar com HTML complexo ou mal formado. Felizmente, bibliotecas como "golang.org/x/net/html" em Go facilitam o parsing de HTML de maneira robusta.

Existem alternativas como BeautifulSoup em Python ou Nokogiri em Ruby, mas a vantagem do Go está em sua eficiência e facilidade ao lidar com concorrência.

Os detalhes de implementação envolvem a tokenização do HTML e o uso de um analisador sintático que pode construir uma árvore DOM-like a partir da qual podemos extrair e manipular dados.

## Veja Também

- Pacote Go "net/html" [golang.org/x/net/html](https://pkg.go.dev/golang.org/x/net/html)
- Documentação do Go [golang.org](https://golang.org/doc/)