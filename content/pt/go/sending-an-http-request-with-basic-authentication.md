---
title:                "Go: Enviando uma requisição http com autenticação básica"
simple_title:         "Enviando uma requisição http com autenticação básica"
programming_language: "Go"
category:             "Go"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/go/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## Por que enviar uma solicitação HTTP com autenticação básica?

A autenticação básica é um método de autenticação amplamente utilizado na web, especialmente em sistemas de login. Enviar uma solicitação HTTP com autenticação básica garante que apenas usuários autorizados possam acessar determinados recursos ou informações.

## Como fazer

Em Go, podemos enviar uma solicitação HTTP com autenticação básica utilizando a estrutura `http.Client` e o pacote `encoding/base64` para codificar as credenciais em um cabeçalho `Authorization`. Veja um exemplo de como podemos enviar uma solicitação GET com autenticação básica:

```Go
package main

import (
	"fmt"
	"net/http"
	"encoding/base64"
)

func main() {
	// Definir URL alvo e credenciais de autenticação
	url := "https://meudominio.com/login"
	username := "usuario"
	password := "senha"

	// Codificar as credenciais em base64
	auth := username + ":" + password
	base64Auth := base64.StdEncoding.EncodeToString([]byte(auth))

	// Criar cabeçalho Authorization
	basicAuth := "Basic " + base64Auth

	// Criar cliente HTTP e adicionar cabeçalho
	client := &http.Client{}
	req, err := http.NewRequest("GET", url, nil)
	req.Header.Add("Authorization", basicAuth)

	// Enviar solicitação
	resp, err := client.Do(req)
	if err != nil {
		fmt.Println("Erro ao enviar solicitação:", err)
	}
	defer resp.Body.Close()

	// Imprimir resposta
	fmt.Println(resp.Status)
}
```
A saída seria algo como `200 OK`, indicando que a solicitação foi bem sucedida.

## Mergulho Profundo

Além de enviar uma solicitação GET, podemos também enviar solicitações POST, PUT, PATCH ou DELETE com autenticação básica. Além disso, podemos acessar as informações do cabeçalho de resposta para validar se a solicitação foi autorizada ou não.

É importante lembrar que a autenticação básica não é considerada uma forma segura de proteger recursos sensíveis. Recomenda-se o uso de métodos mais robustos, como OAuth ou JWT, para garantir a segurança das suas aplicações web.

## Veja Também

- [Pacote `encoding/base64` da documentação oficial do Go](https://golang.org/pkg/encoding/base64/)
- [Tutorial de autenticação HTTP básica em Go por Eli Bendersky](https://eli.thegreenplace.net/2021/password-protected-http-endpoints-in-go/)
- [Artigo sobre autenticação básica na RFC 7617](https://tools.ietf.org/html/rfc7617)