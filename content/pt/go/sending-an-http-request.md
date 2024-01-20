---
title:                "Enviando uma solicitação http"
html_title:           "Bash: Enviando uma solicitação http"
simple_title:         "Enviando uma solicitação http"
programming_language: "Go"
category:             "Go"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/go/sending-an-http-request.md"
---

{{< edit_this_page >}}

---

## O Que & Porquê?

A realização de um pedido HTTP é a principal maneira de os browsers e aplicações client-side recuperem dados de um servidor. Como programadores, nós fazemos isso para buscar dados e interagir com APIs ou serviços baseados na web.

## Como fazer:

Vamos entrar no mundo do Go:

```Go
package main

import (
	"log"
	"net/http"
	"io/ioutil"
)

func main() {
	res, err := http.Get("http://example.com")
	if err != nil {
		log.Fatal(err)
	}
	defer res.Body.Close()
	body, err := ioutil.ReadAll(res.Body)
	if err != nil {
		log.Fatal(err)
	}
	log.Println(string(body))
}
```
Na execução deste código, enviaremos um GET request para "http://example.com" e obteremos a resposta. É simples e direto.

## Mergulhando fundo:

Para enviar um pedido HTTP em Go, utilizamos o pacote net/http, que fornece funcionalidades HTTP cliente e servidor. A função http.Get é muito utilizada e é uma chamada de alto nível que nos ajuda a fazer um pedido HTTP muito rapidamente.

Historicamente, Go tornou-se popular para o desenvolvimento backend devido ao seu forte suporte para concorrência e sua rica biblioteca padrão. Existem alternativas para a solicitação de rede em Go, como o uso de pacotes de terceiros (por exemplo, o pacote `goreq`), mas o pacote `net/http` é mais comumente utilizado devido à sua simplicidade e eficácia.

Quanto aos detalhes de implementação, quando você emite uma solicitação GET, o Go abrirá uma conexão TCP com o servidor e enviará o pedido HTTP. O servidor responderá com um texto, que é então lido, decodificado e retornado para você como uma resposta.

## Veja também:

- [Documentação Go net/http](https://golang.org/pkg/net/http/)
- [Um tutorial mais detalhado sobre requisições HTTP em Go](https://medium.com/rungo/making-external-http-requests-in-go-eb4c015f8839)

---