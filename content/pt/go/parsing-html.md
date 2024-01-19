---
title:                "Analisando HTML"
html_title:           "Arduino: Analisando HTML"
simple_title:         "Analisando HTML"
programming_language: "Go"
category:             "Go"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/go/parsing-html.md"
---

{{< edit_this_page >}}

## O Que & Por Quê?

Analisar HTML (parsing HTML) é a prática de decifrar e entender o código HTML. Os programadores fazem isso para interagir, manipular ou extrair dados da web e para entender melhor a estrutura das páginas da web.

## Como Fazer

Go oferece vários pacotes úteis para a análise de HTML. Vamos utilizar o pacote goquery, que permite uma sintaxe similar ao jQuery para manipulação e iteração na árvore DOM. 

Para instalar o pacote goquery, use o comando:

```Go 
go get github.com/PuerkitoBio/goquery
```

Vamos criar um exemplo simples para extrair todos os links de uma página HTML:

```Go 
package main

import (
	"fmt"
	"github.com/PuerkitoBio/goquery"
	"log"
	"net/http"
)

func main() {
	// Obtém a resposta HTTP
	res, err := http.Get("http://golang.org")
	if err != nil {
		log.Fatal(err)
	}
	defer res.Body.Close()

	// Cria um documento goquery a partir da resposta HTTP
	doc, err := goquery.NewDocumentFromReader(res.Body)
	if err != nil {
		log.Fatal(err)
	}

	// Encontra e imprime todos os links
	doc.Find("a").Each(func(index int, item *goquery.Selection) {
		linkTag := item
		link, _ := linkTag.Attr("href")
		fmt.Println(link)
	})
}
```

O código acima extrai todos os links da página inicial do Golang.


## Mergulho Profundo

A análise HTML tem suas raízes na linguagem de marcação HTML e a necessidade de interagir com ela. É uma técnica comum usada em web scraping para extrair dados diretamente de páginas da web. Existem alternativas ao goquery, como o pacote nativo 'net/html' do Go, mas o goquery fornece uma API mais fácil de usar, baseada na popular biblioteca jQuery.

Quando se trata de análise HTML, o código não gera apenas uma lista de tags e texto. Em vez disso, ele cria uma estrutura de árvore denominada Document Object Model (DOM), o que permite uma fácil navegação e manipulação. Mas lembre-se, lidar com páginas HTML mal formatadas pode ser complicado, pois a análise HTML depende de uma estrutura de tags de abertura e fechamento corretas.

## Veja Também

1. Documentação oficial do pacote goquery: https://github.com/PuerkitoBio/goquery
2. Tutorial Go Web Scraping: https://edmundmartin.com/writing-a-web-crawler-in-golang/
3. Documentação HTML/net do Go: https://pkg.go.dev/net/html