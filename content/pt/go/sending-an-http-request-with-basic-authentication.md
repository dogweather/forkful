---
title:                "Enviando uma solicitação http com autenticação básica"
html_title:           "Clojure: Enviando uma solicitação http com autenticação básica"
simple_title:         "Enviando uma solicitação http com autenticação básica"
programming_language: "Go"
category:             "Go"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/go/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

# Enviando uma Solicitação HTTP com Autenticação Básica em Go

## O Que & Por Quê?

Enviar uma solicitação HTTP com autenticação básica é o processo de envio de dados para um servidor web com credenciais de login. Isso é feito para garantir que o remetente é autorizado e proteger os dados sendo enviados.

## Como Fazer:

Vamos começar enviando uma solicitação HTTP básica. Aqui está o código em Go:

```Go
package main

import (
	"net/http"
	"fmt"
)

func main() {
	req, _ := http.NewRequest("GET", "http://exemplo.com", nil)
	req.SetBasicAuth("usuario", "senha")

	res, _ := http.DefaultClient.Do(req)
	fmt.Println(res.Status)
}
```

Quando você executa esse código, você verá a resposta do status HTTP do seu servidor na linha de comando.

## Deep Dive

A autenticação básica HTTP é um método antigo, mas simples, para segurança na web. Historicamente é usado quando uma proteção simples para dados sensíveis é necessária e SSL/TLS não é necessário. Hoje em dia, é melhor utilizar a autenticação básica apenas em conjunto com SSL/TLS, já que as credenciais são transmitidas como texto simples.

Existem várias alternativas à autenticação básica, como OAuth e JWT, que oferecem uma variedade de recursos de segurança avançados. Implementá-los pode ser um pouco mais complexo, então a preferência realmente depende das necessidades do seu projeto.

Internamente, a autenticação básica em Go ocorre pela adição de um cabeçalho 'Authorization' à solicitação HTTP. O valor deste cabeçalho é composto pela palavra 'Basic' seguida de um espaço e da combinação base64 da string "usuario:senha".

## Veja Também

* Documentação oficial do Go para [autenticação básica](https://golang.org/pkg/net/http/#Request.SetBasicAuth)
* Leia mais sobre [OAuth](https://oauth.net/)
* Leia mais sobre [JWT](https://jwt.io/introduction/)
* Uma discussão detalhada sobre [autenticação HTTP básica vs. token](https://security.stackexchange.com/questions/108662/basic-http-auth-vs-token-based-auth)