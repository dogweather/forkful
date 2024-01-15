---
title:                "Baixando uma página da web"
html_title:           "Go: Baixando uma página da web"
simple_title:         "Baixando uma página da web"
programming_language: "Go"
category:             "Go"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/go/downloading-a-web-page.md"
---

{{< edit_this_page >}}

## Por que

Se você está se aventurando no mundo da programação, é provável que já tenha ouvido falar sobre a linguagem de programação Go, também conhecida como Golang. Mas você sabia que uma das funcionalidades do Go é a capacidade de fazer o download de páginas da web? Isso mesmo, com apenas algumas linhas de código, você pode baixar o conteúdo de uma página da web e utilizá-lo em suas aplicações. Mas por que alguém iria querer fazer isso?

Bom, existem diversas razões para fazer o download de uma página da web. Talvez você queira criar um web crawler para coletar dados de diferentes sites, ou talvez precise acessar informações específicas de uma página para sua aplicação. Independentemente do motivo, aprender a fazer o download de páginas da web pode ser extremamente útil para seus projetos.

## Como fazer

Para fazer o download de uma página da web em Go, você precisará utilizar a biblioteca padrão `net/http`. Vamos ver um exemplo de código que faz o download do conteúdo da página inicial do Google:

```Go
package main

import (
    "fmt"
    "io/ioutil"
    "net/http"
)

func main() {
    url := "https://www.google.com"
    response, err := http.Get(url)
    if err != nil {
        panic(err)
    }
    defer response.Body.Close()

    body, err := ioutil.ReadAll(response.Body)
    if err != nil {
        panic(err)
    }

    fmt.Println(string(body))
}
```

Vamos explicar um pouco do que está acontecendo aqui. Primeiro, importamos as bibliotecas necessárias para nosso programa. Em seguida, definimos a URL que queremos fazer o download no formato de string. Utilizamos a função `http.Get()` para fazer a requisição GET para a página e armazenamos a resposta em uma variável `response`. Em seguida, utilizamos `ioutil` para ler o conteúdo do corpo da resposta e imprimimos o resultado no console.

Ao rodar esse código, você verá o conteúdo da página do Google sendo impresso na tela. Claro que esse é apenas um exemplo básico, mas já mostra como é simples fazer o download de páginas da web em Go. Você pode alterar a URL para a página que quiser e manipular o corpo da resposta da maneira que preferir.

## Deep Dive

Se você quiser se aprofundar e entender melhor como funciona o processo de download de páginas da web em Go, existem alguns conceitos importantes que você precisará compreender. A primeira coisa é entender como funciona o protocolo HTTP e como fazer requisições para uma URL específica.

Além disso, é importante estar familiarizado com a biblioteca `net/http` e suas funcionalidades, como a função `http.Get()` que utilizamos no exemplo acima. Também é importante saber lidar com possíveis erros e tratar as respostas de acordo com a estrutura do objeto `Response` da biblioteca.

## Veja também

- [Documentação da biblioteca `net/http`](https://golang.org/pkg/net/http/)
- [Tutorial da linguagem Go no site oficial](https://golang.org/doc/)
- [Outro tutorial sobre como fazer o download de páginas da web em Go](https://programming.vip/docs/how-to-download-a-web-page-in-golang.html)