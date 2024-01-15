---
title:                "Enviando uma solicitação http"
html_title:           "Elm: Enviando uma solicitação http"
simple_title:         "Enviando uma solicitação http"
programming_language: "Elm"
category:             "Elm"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/elm/sending-an-http-request.md"
---

{{< edit_this_page >}}

## Por que enviar uma requisição HTTP em Elm?

Em muitos aplicativos da web, precisamos enviar e receber informações do servidor. Para isso, usamos o protocolo HTTP e, em Elm, podemos fazer isso facilmente com a biblioteca Http. Neste artigo, vamos aprender como enviar uma solicitação HTTP em Elm e como lidar com a resposta.

## Como Fazer?

Para começar, primeiro importamos a biblioteca Http:

```Elm
import Http
```

Em seguida, usamos a função `send` para enviar uma solicitação. Esta função recebe três argumentos: o método HTTP que queremos usar, a URL para onde queremos enviar a solicitação e os dados que queremos enviar. Por exemplo, para enviar um POST para `www.exemplo.com` com os dados `{nome: "João", idade: 30}`:

```Elm
Http.send Http.post "http://www.exemplo.com" (Http.jsonBody "{nome: "João", idade: 30}")
```

Nota: Para usar a função `send`, o tipo de dados dos nossos dados deve ser convertido para `Value` usando `Http.jsonBody`.

Para lidar com a resposta, usamos a função `send`, que recebe dois argumentos: uma função para lidar com a resposta bem-sucedida e uma função para lidar com a resposta com erro. Por exemplo, para imprimir o corpo da resposta bem-sucedida no console e lidar com um erro com uma mensagem de erro:

```Elm
Http.send onSuccess onError
    where
        onSuccess response =
            case response.body of
                Http.Success responseBody ->
                    log responseBody

                Http.Failure _ ->
                    onError "Erro na requisição!"
        
        onError errorMessage =
            log errorMessage
```

## Aprofundando

A biblioteca Http em Elm oferece muitos outros recursos para lidar com solicitações HTTP. Alguns deles são:

- `expectJson`: usado para analisar a resposta em JSON e lidar com possíveis erros de análise.
- `expectString`: usado para analisar a resposta em uma string simples.
- `expectBytes`: usado para analisar a resposta em bytes.
- `multipartBody`: usado para enviar dados em formato multipart/form-data, como o upload de arquivos.
- `toTask`: usado para transformar uma solicitação HTTP em uma tarefa que pode ser executada em série ou paralelamente com outras tarefas.

Com esses recursos, podemos personalizar ainda mais nossas solicitações HTTP de acordo com as necessidades do nosso aplicativo.

## Veja também

- [Documentação oficial da biblioteca Http em Elm](https://package.elm-lang.org/packages/elm/http/latest/)
- [Tutorial de como enviar uma solicitação HTTP em Elm](https://guide.elm-lang.org/effects/http.html)
- [Exemplo de aplicativo Elm usando solicitações HTTP](https://github.com/elm/http/blob/master/examples/elm/Main.elm)

Agora que você aprendeu como enviar uma solicitação HTTP em Elm, experimente criar seu próprio projeto e implementar esses conceitos para criar uma aplicação web poderosa e robusta.