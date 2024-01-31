---
title:                "Enviando uma requisição HTTP com autenticação básica"
date:                  2024-01-20T18:01:36.745430-07:00
model:                 gpt-4-1106-preview
simple_title:         "Enviando uma requisição HTTP com autenticação básica"

category:             "Go"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/go/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## What & Why?
Fazer uma requisição HTTP com autenticação básica significa enviar um pedido a um servidor que exige usuário e senha. Programadores fazem isso para acessar recursos protegidos em APIs ou outros serviços web.

## How to:
```Go
package main

import (
    "encoding/base64"
    "fmt"
    "net/http"
    "io/ioutil"
)

func main() {
    client := &http.Client{}
    req, err := http.NewRequest("GET", "http://exemplo.com/recurso", nil)
    if err != nil {
        panic(err)
    }
    
    // Codificação do usuário e senha
    auth := base64.StdEncoding.EncodeToString([]byte("usuario:senha"))

    // Adicionar o cabeçalho de autenticação
    req.Header.Add("Authorization", "Basic " + auth)

    // Fazer a requisição
    resp, err := client.Do(req)
    if err != nil {
        panic(err)
    }
    defer resp.Body.Close()

    // Ler e imprimir a resposta
    body, err := ioutil.ReadAll(resp.Body)
    if err != nil {
        panic(err)
    }
    fmt.Println(string(body))
}
```

## Deep Dive
A autenticação básica HTTP é um método antigo, mas direto, parte do protocolo HTTP 1.0 desde 1996. Funciona codificando 'username:password' em Base64 e inserindo na cabeça do pedido. Apesar de simples, não é a mais segura; por isso, sempre use HTTPS, que criptografa a comunicação. Existem métodos mais modernos como OAuth, JWT e outras formas de autenticação de tokens, mas a autenticação básica ainda é comum devido à sua simplicidade. Quando implementar, certifique-se de lidar com a codificação e decodificação corretamente e de manejar os cabeçalhos HTTP.

## See Also
- [Go net/http package](https://pkg.go.dev/net/http)
- [Go base64 encoding](https://pkg.go.dev/encoding/base64)
- [HTTP Basic Authentication Scheme](https://tools.ietf.org/html/rfc7617)
- [All About HTTP Authentication and Security (em inglês)](https://developer.mozilla.org/en-US/docs/Web/HTTP/Authentication)
