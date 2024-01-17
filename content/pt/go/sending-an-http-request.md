---
title:                "Enviando uma requisição http"
html_title:           "Go: Enviando uma requisição http"
simple_title:         "Enviando uma requisição http"
programming_language: "Go"
category:             "Go"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/go/sending-an-http-request.md"
---

{{< edit_this_page >}}

## O que & Por que?

Enviar uma solicitação HTTP é o processo de enviar uma requisição a um servidor para obter informações ou executar uma ação. Programadores fazem isso para acessar recursos online, realizar integrações com outras aplicações ou atualizar dados em uma base de dados. É uma parte essencial do desenvolvimento de aplicações modernas em que a comunicação com servidores é crucial.

## Como fazer:

```Go
resp, err := http.Get("http://www.example.com")
if err != nil {
    // tratar erro 
}
defer resp.Body.Close()

body, err := ioutil.ReadAll(resp.Body)
if err != nil {
    // tratar erro
}

fmt.Println(string(body))

// Saída: "<html><head><title>Exemplo</title></head><body>Bem vindo ao exemplo!</body></html>"
```

## Deep Dive:

Enviar solicitações HTTP é algo que tem sido feito há décadas, mas com a evolução da tecnologia, tornou-se mais importante do que nunca. Existem várias bibliotecas e ferramentas disponíveis para enviar solicitações HTTP em diversas linguagens de programação, mas o Go tem uma biblioteca padrão incorporada que torna o processo simples e eficiente. Além disso, com o uso de rotinas e canais, é possível paralelizar e tornar mais rápido o envio de várias solicitações simultaneamente. Alternativas incluem o uso de pacotes externos, como o "gorilla/mux" para adicionar mais funcionalidades à biblioteca padrão do Go.

## Veja também:

- https://pkg.go.dev/net/http
- https://github.com/gorilla/mux