---
title:                "Enviando uma solicitação http"
html_title:           "Bash: Enviando uma solicitação http"
simple_title:         "Enviando uma solicitação http"
programming_language: "Elm"
category:             "Elm"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/elm/sending-an-http-request.md"
---

{{< edit_this_page >}}

## O Que & Por Quê?
Enviar uma requisição HTTP é o processo de solicitar dados de um servidor ou enviar dados para um servidor, utilizando o Protocolo de Transferência de Hipertexto (HTTP). Programadores fazem isso para trocar dados entre o cliente e o servidor.

## Como fazer:
O exemplo a seguir mostra como enviar uma requisição GET usando Elm:

```Elm
import Http
import Json.Decode as Decode

getDados : String -> Cmd Msg
getDados url =
    Http.get
        { url = url
        , expect = Http.expectString GotDados
        }

type Msg 
    = GotDados (Result Http.Error String)
```
Quando executamos o comando 'getDados', ele retorna um comando que o Elm irá executar para nos. O resultado é então passado para a nossa função 'GotDados'.

## Mergulho Profundo
Elm está enviando requisições HTTP desde o seu lançamento em 2012. Embora o jeito Elm de lidar com HTTP possa parecer diferente se você estiver acostumado com JavaScript, a abordagem centrada em segurança e facilidade de uso do Elm resulta em um código previsível e fácil de manutenção.

Existem outras maneiras e bibliotecas para lidar com HTTP em Elm, como `elm-http-builder` e `elm-http-extra`, mas `elm/http` é a mais direta e normalmente a mais utilizada. Vale a pena explorar as alternativas se você precisar de funcionalidades mais avançadas.

Quando enviamos uma requisição GET, como no exemplo acima, Elm cria uma tarefa que é enviada para o sistema de tempo de execução do Elm, que cuida de executar a tarefa para nós e devolver o resultado quando estiver pronto.

## Veja Mais
Aqui estão alguns recursos adicionais se você quiser mergulhar mais fundo em HTTP com Elm:
- A [documentação oficial do Http](https://package.elm-lang.org/packages/elm/http/latest) é um ótimo lugar para começar.
- O [guia oficial](https://guide.elm-lang.org/) tem um capítulo inteiro dedicado a interações com o servidor, incluindo como trabalhar com JSON.