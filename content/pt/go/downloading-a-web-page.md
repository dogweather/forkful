---
title:                "Baixando uma página web"
html_title:           "Go: Baixando uma página web"
simple_title:         "Baixando uma página web"
programming_language: "Go"
category:             "Go"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/go/downloading-a-web-page.md"
---

{{< edit_this_page >}}

## O que e Por Que?

Baixar uma página da web é o processo de obter o conteúdo de uma página da internet e armazená-lo em seu computador. Os programadores frequentemente fazem isso para automatizar tarefas, como extrair dados ou analisar informações.

## Como Fazer:

Um método simples para baixar uma página da web usando Go é usar a biblioteca padrão "net/http". Veja o código abaixo para um exemplo básico:

```
package main

import (
	"fmt"
	"io/ioutil"
	"net/http"
)

func main() {
	resp, err := http.Get("https://google.com")
	if err != nil {
		fmt.Println("Erro ao fazer o download:", err)
		return
	}

	defer resp.Body.Close()

	body, err := ioutil.ReadAll(resp.Body)
	if err != nil {
		fmt.Println("Erro ao ler a resposta:", err)
		return
	}

	fmt.Println(string(body))
}
```

Este código utiliza a função `Get` da biblioteca `net/http` para fazer uma requisição à página do Google. Em seguida, o corpo da resposta é lido e impresso na tela. É importante notar o uso do `defer` para fechar o corpo da resposta após seu uso.

## Profundando o Assunto:

Baixar páginas da web é uma tarefa comum em vários cenários, como web scraping e desenvolvimento de aplicativos. Existem diversas bibliotecas em Go que oferecem recursos mais avançados, como a capacidade de lidar com requisições assíncronas, cacheamento de dados e suporte a proxies.

Além disso, também é possível utilizar ferramentas de terceiros, como o cURL, para fazer o download de páginas da web em Go.

## Veja Também:

- Documentação da biblioteca "net/http": https://golang.org/pkg/net/http/
- Tutorial sobre web scraping em Go: https://www.alexedwards.net/blog/scraping-in-go
- Ferramenta de linha de comando cURL: https://curl.haxx.se/docs/manpage.html