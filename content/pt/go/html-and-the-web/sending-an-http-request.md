---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 18:08:39.714256-07:00
description: "Como fazer: Em Go, enviar uma requisi\xE7\xE3o HTTP e lidar com a resposta\
  \ envolve o uso do pacote `net/http`. Aqui est\xE1 um exemplo passo a passo mostrando\
  \ como\u2026"
lastmod: '2024-03-13T22:44:46.057169-06:00'
model: gpt-4-0125-preview
summary: "Em Go, enviar uma requisi\xE7\xE3o HTTP e lidar com a resposta envolve o\
  \ uso do pacote `net/http`."
title: "Enviando uma solicita\xE7\xE3o HTTP"
weight: 44
---

## Como fazer:
Em Go, enviar uma requisição HTTP e lidar com a resposta envolve o uso do pacote `net/http`. Aqui está um exemplo passo a passo mostrando como enviar uma simples requisição GET e ler a resposta:

```go
package main

import (
    "fmt"
    "io/ioutil"
    "log"
    "net/http"
)

func main() {
    // Define a URL do recurso
    url := "http://example.com"

    // Use http.Get para enviar a requisição GET
    resp, err := http.Get(url)
    if err != nil {
        log.Fatal(err)
    }
    // Fecha o corpo da resposta quando a função terminar
    defer resp.Body.Close()

    // Leia o corpo da resposta
    body, err := ioutil.ReadAll(resp.Body)
    if err != nil {
        log.Fatal(err)
    }

    // Converte o corpo da resposta para uma string e imprime
    fmt.Println(string(body))
}
```

Saída de exemplo (abreviada para brevidade):
```
<!doctype html>
<html>
<head>
    <title>Domínio de Exemplo</title>
...
</html>
```

Para enviar uma requisição POST com dados de formulário, você pode usar `http.PostForm`:

```go
package main

import (
    "fmt"
    "io/ioutil"
    "net/http"
    "net/url"
)

func main() {
    // Define a URL e os dados do formulário
    url := "http://example.com/form"
    data := url.Values{}
    data.Set("chave", "valor")

    // Envia a requisição POST com dados do formulário
    resp, err := http.PostForm(url, data)
    if err != nil {
        panic(err)
    }
    defer resp.Body.Close()

    // Lê e imprime a resposta
    body, err := ioutil.ReadAll(resp.Body)
    if err != nil {
        panic(err)
    }

    fmt.Println(string(body))
}
```

## Aprofundamento
O pacote `net/http` em Go oferece uma forma poderosa e flexível de interagir com servidores HTTP. Seu design reflete a ênfase de Go em simplicidade, eficiência e robustez. Originalmente, funcionalidades como lidar com cargas JSON ou XML requeriam a elaboração manual do corpo da requisição e a configuração de cabeçalhos apropriados. Conforme Go evoluiu, a comunidade desenvolveu pacotes de nível mais alto que simplificam ainda mais essas tarefas, como `gorilla/mux` para roteamento e `gjson` para manipulação de JSON.

Um aspecto notável do cliente HTTP de Go é seu uso de interfaces e structs, como `http.Client` e `http.Request`, que permitem uma personalização extensa e testes. Por exemplo, você pode modificar o `http.Client` para ter tempo limite nas requisições ou manter conexões ativas para desempenho.

Uma alternativa considerada para interações HTTP mais simples é usar bibliotecas de terceiros, como "Resty" ou "Gentleman". Esses pacotes oferecem uma abstração de nível mais alto para requisições HTTP, tornando tarefas comuns mais concisas. No entanto, entender e utilizar o pacote `net/http` subjacente é crucial para lidar com cenários de interação HTTP mais complexos ou únicos, fornecendo uma base sobre a qual as características de concorrência de Go e a poderosa biblioteca padrão podem ser totalmente aproveitadas.
