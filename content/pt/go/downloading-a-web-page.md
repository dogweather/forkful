---
title:                "Baixando uma página da web"
date:                  2024-01-20T17:44:13.586581-07:00
model:                 gpt-4-1106-preview
simple_title:         "Baixando uma página da web"
programming_language: "Go"
category:             "Go"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/go/downloading-a-web-page.md"
---

{{< edit_this_page >}}

## O que é & Porquê?

Baixar uma página da web significa fazer o download do seu conteúdo HTML. Programadores fazem isso para análise de dados, testes de aplicações web, e para monitorar alterações no conteúdo de um site.

## Como Fazer:

```Go
package main

import (
    "fmt"
    "io/ioutil"
    "net/http"
)

func main() {
    response, err := http.Get("http://example.com")
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

Exemplo de saída (apenas o início do HTML):
```
<!doctype html>
<html>
<head>
    <title>Example Domain</title>
...
```

## Aprofundando

Originalmente, baixar uma página da web era feito através de comandos como `wget` ou `curl` em sistemas Unix. Ainda é uma opção, mas a programação oferece mais controle. Com Go, usamos o pacote `net/http` para fazer requisições web. Existem alternativas como `gorilla/http` ou frameworks como `Colly` para scraping mais avançado.

Sobre detalhes de implementação, considere lidar com cookies, redirecionamentos e cabeçalhos HTTP. A função `http.Get` é apenas uma conveniência em volta do `http.Request`. Além disso, `ioutil.ReadAll` lê o corpo todo numa string, mas para grandes respostas, melhor usar streams ou buffers.

## Veja Também

- Documentação Go sobre o pacote `net/http`: https://pkg.go.dev/net/http
- Tutorial Go sobre scraping web com Colly: https://go-colly.org/docs/introduction/start/
- Uso avançado de HTTP em Go: https://blog.golang.org/http-tracing