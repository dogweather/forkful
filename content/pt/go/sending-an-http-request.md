---
title:                "Go: Enviando uma solicitação http"
simple_title:         "Enviando uma solicitação http"
programming_language: "Go"
category:             "Go"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/go/sending-an-http-request.md"
---

{{< edit_this_page >}}

## Por que enviar uma solicitação HTTP em Go?

Enviar solicitações HTTP é uma parte essencial do desenvolvimento de aplicativos web em Go. Essas solicitações permitem que nosso aplicativo se comunique com outros servidores ou APIs, tornando possível a troca de dados e informações. Sem enviar uma solicitação HTTP, nosso aplicativo ficaria isolado e incapaz de se integrar ao mundo maior da web.

## Como enviar uma solicitação HTTP em Go?

Para enviar uma solicitação HTTP em Go, precisamos utilizar a biblioteca padrão "net/http". Primeiro, precisamos criar um cliente HTTP, que será responsável por enviar a solicitação. Em seguida, usaremos a função "Get" ou "Post" do cliente para enviar uma solicitação HTTP para um determinado URL. Confira o código de exemplo abaixo:

```Go
package main

import (
    "fmt"
    "net/http"
    "io/ioutil"
)

func main() {
    // Criando um cliente HTTP
    client := &http.Client{}

    // Enviando uma solicitação GET para o URL "https://jsonplaceholder.typicode.com/posts/1"
    response, err := client.Get("https://jsonplaceholder.typicode.com/posts/1")
    if err != nil {
        // Manuseando qualquer erro
        fmt.Println("Erro ao enviar a solicitação HTTP:", err)
    }
    defer response.Body.Close()

    // Lendo o conteúdo da resposta
    body, err := ioutil.ReadAll(response.Body)
    if err != nil {
        // Manuseando qualquer erro
        fmt.Println("Erro ao ler o corpo da resposta:", err)
    }
    // Exibindo o resultado
    fmt.Println(string(body))
}

```

Esse código envia uma solicitação GET para o servidor da API JSON Placeholder e imprime o conteúdo da resposta no console. Experimente executar o código e verifique a saída!

## Aprofundando nos detalhes das solicitações HTTP

Existem várias propriedades de uma solicitação HTTP que podemos controlar em Go. Podemos definir o cabeçalho da solicitação para incluir informações adicionais, definir o método da solicitação (como GET, POST, PUT, DELETE, etc.) e até mesmo definir um corpo de dados para ser enviado com a solicitação. Também podemos controlar os cookies e outras configurações da solicitação usando a estrutura "Request" da biblioteca "net/http". Além disso, podemos analisar a resposta retornada pelo servidor para obter mais informações e lidar com diferentes códigos de status. Explore a documentação oficial do pacote "net/http" para obter mais detalhes sobre esses recursos.

## Veja também

- Documentação da biblioteca "net/http" do Go: https://golang.org/pkg/net/http/
- Tutorial de solicitações HTTP em Go: https://medium.com/@masnun/making-http-requests-in-golang-dd123379efe7
- Exemplo de envio de solicitações HTTP em Go: https://gobyexample.com/http-clients