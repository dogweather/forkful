---
title:                "Enviando uma solicitação http"
html_title:           "Go: Enviando uma solicitação http"
simple_title:         "Enviando uma solicitação http"
programming_language: "Go"
category:             "Go"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/go/sending-an-http-request.md"
---

{{< edit_this_page >}}

## Por que enviar uma solicitação HTTP com Go?

Enviar uma solicitação HTTP é uma tarefa comum ao criar aplicativos web ou APIs. Com Go, essa tarefa é facilitada, pois possui uma biblioteca padrão poderosa para lidar com requisições e respostas HTTP.

## Como fazer?

Para enviar uma solicitação HTTP com Go, primeiro precisamos importar o pacote "net/http". Em seguida, usamos a função "Get" desse pacote, passando a URL do endpoint desejado como parâmetro. O código ficaria assim:

```Go
import "net/http"

func main() {
    resp, err := http.Get("https://exemplo.com/api/usuarios")
    if err != nil {
        // tratamento de erros
    }
    defer resp.Body.Close()

    // código para lidar com a resposta
}
```

Ao executar esse código, uma solicitação GET será enviada para a URL fornecida e a resposta será armazenada na variável "resp". No exemplo acima, estamos apenas acessando a resposta, mas é possível manipulá-la de diversas maneiras, como converter para um tipo específico de dado ou decodificar dados JSON.

## Aprofundando-se

A biblioteca "net/http" oferece muitas opções para customizar e controlar a forma como as solicitações são enviadas e respostas são recebidas. Algumas funções importantes que podemos explorar são:

- "Post": para enviar uma solicitação do tipo POST;
- "Client": para criar um cliente personalizado com configurações específicas;
- "Transport": para definir o comportamento do transporte da solicitação, como uso de caching ou proxies.

Recomendamos explorar a documentação oficial da biblioteca para aprender mais sobre esses recursos e como utilizá-los em suas aplicações.

## Veja também

- Documentação oficial da biblioteca "net/http": https://pkg.go.dev/net/http
- Exemplos de uso da biblioteca em projetos reais: https://github.com/golang/go/wiki/Projects#web-applications