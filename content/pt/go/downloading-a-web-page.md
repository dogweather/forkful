---
title:                "Baixando uma página da web"
html_title:           "Bash: Baixando uma página da web"
simple_title:         "Baixando uma página da web"
programming_language: "Go"
category:             "Go"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/go/downloading-a-web-page.md"
---

{{< edit_this_page >}}

## O Que & Porquê?

Baixar uma página web envolve recuperar e armazenar o conteúdo HTML para uso ou análise futura. Os programadores fazem isso para analisar e usar os dados presentes no HTML ou para arquivar o conteúdo da web.

## Como Fazer:

O pacote `net/http` em Go facilita o download de páginas web. Criamos um novo pedido usando `http.NewRequest` e, em seguida, usamos `http.DefaultClient.Do` para enviar o pedido.

```Go
package main

import (
	"io/ioutil"
	"net/http"
	"os"
)

func main() {
	resp, err := http.Get("http://example.com")
	if err != nil {
		os.Exit(1)
	}
	defer resp.Body.Close()
	body, err := ioutil.ReadAll(resp.Body)
	if err != nil {
		os.Exit(1)
	}
	println(string(body))
}
```
Quando executamos este programa, veremos o conteúdo HTML da página web exemplo.com impresso no terminal.

## Aprofundando

Historicamente, as bibliotecas como Curl e Wget têm sido usadas para baixar páginas da web. Em Go, `http.Get` fornece funcionalidades semelhantes de maneira mais simples e idiomática.

Existem outras bibliotecas em Go, como `colly`, que oferecem mais recursos, como parsing de HTML e suporte para escalonamento de web.

Ao baixar uma página web, as respostas HTTP são lidas como byte streams. Podemos convertê-las em strings para torná-las legíveis para os humanos.

## Veja Também

Fontes para aprender mais e explorar sobre o download de páginas web:

1. Pacote http do Go: https://pkg.go.dev/net/http
2. Biblioteca colly: https://github.com/gocolly/colly
3. HTTP Overview: https://developer.mozilla.org/pt-BR/docs/Web/HTTP/Overview