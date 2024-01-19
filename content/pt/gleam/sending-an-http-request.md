---
title:                "Enviando uma solicitação http"
html_title:           "Bash: Enviando uma solicitação http"
simple_title:         "Enviando uma solicitação http"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/gleam/sending-an-http-request.md"
---

{{< edit_this_page >}}

## Qual & Porquê?

Enviar uma solicitação HTTP significa fazer um pedido a um servidor web. Programadores fazem isso para interagir com serviços baseados na web, por exemplo, para obter dados, enviar dados, ou deletar dados em um servidor.

## Como Fazer:

No Gleam, você usará módulos como `httpc` e `http_uri` dentro da biblioteca padrão (`gleam/http`). Aqui está um exemplo de como fazer um pedido GET:

```Gleam
import gleam/http.{Get, httpc}
import gleam/http_uri.{parse}

fn enviar_solicitacao() {
  let url = parse("http://seu_servico_web.com/endpoint")?
  let resposta = httpc.send(Get(url))
  case resposta {
    Ok(resposta) -> io.println(resposta.body)
    Error(_) -> io.println("Não foi possível enviar a solicitação HTTP. Verifique a URL e tente novamente.")
  }
}
```

Quando executado, isso enviará uma solicitação GET para `http://seu_servico_web.com/endpoint` e imprimirá a resposta, ou um erro se a solicitação falhou.

## Explicação Detalhada

O protocolo HTTP foi padronizado no início dos anos 90 e é a base da comunicação na Internet. No Gleam, a HttpClient é implementada como uma fina interface Erlang. 

Há muitas alternativas para fazer uma solicitação HTTP. No mundo do Erlang / Elixer, existem bibliotecas como `hackney` e `http_poison`. No entanto, a biblioteca `gleam/http` fornece uma interface mais idiomática para programadores Gleam.

Como mencionado antes, a classe `httpc` do Gleam é uma interface para o 'httpc' de Erlang. Isso faz parte do aplicativo 'inets' que vem com Erlang / OTP e tem sido mantido e atualizado há muitos anos.

## Ver Também:

- Documentação oficial do Gleam http: https://gleam.run/documentation/http/
- Módulo httpc Erlang: https://erlang.org/doc/man/httpc.html
- História do HTTP: https://www.lifewire.com/history-of-http-3466130
- Módulos e funções Gleam: https://gleam.run/documentation/tour/modules-and-functions/