---
title:                "Baixando uma página da web"
html_title:           "Bash: Baixando uma página da web"
simple_title:         "Baixando uma página da web"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/gleam/downloading-a-web-page.md"
---

{{< edit_this_page >}}

## O Que & Por Que?

Baixar uma página da web é a ação de copiar e armazenar todo o conteúdo de um site em seu dispositivo. Programadores fazem isso para analisar páginas da web, para recolher ou testar dados ou para disponibilizar conteúdo em modo off-line.

## Como:

Em Gleam, você pode baixar páginas da web utilizando a biblioteca httpc do Erlang. Aqui está um exemplo de como você poderia fazer isso:

```Gleam
import erlang.{ok, error}

pub fn download(url: String) -> Result(BitString, String) {
  case erlang.httpc.request("GET".to_string(), url, [], []) {
    | ok(_, _, _, {_, body}) -> Ok(body)
    | error(err) -> Error(err)
  }
}
```
Ao executar este código com um URL válido, o conteúdo da página da web será retornado como um resultado de sucesso. Caso contrário, o erro será retornado como um resultado de falha.

## Mergulho Profundo:

Historicamente, a ação de baixar uma página da web começou quando a internet era em sua maioria statis. Hoje, há várias outras maneiras de baixar uma página web, usando linguagens de programação diferentes ou até mesmo frameworks especializados. Em Gleam, ao usar a biblioteca httpc do Erlang, estamos utilizando um módulo desenvolvido para a linguagem Erlang, que é amadurecida e largamente utilizada em sistemas de grande tráfego.

Em contraste, outras alternativas podem incluir o uso de bibliotecas nativas em Gleam (uma vez desenvolvidas), ou outras bibliotecas em Erlang. Dependendo do caso de uso específico, pode ser necessário fazer mais do que apenas baixar uma página da web, talvez parsear o conteúdo ou interagir com ele de alguma maneira. Nesses casos, bibliotecas mais especializadas ou um navegador controlado por código poderia ser mais apropriado.

Em termos de implementação, a biblioteca httpc oferece uma interface simples e direta para realizar solicitações HTTP. Ela lida com muitos dos detalhes de baixo nível, como conexões de rede e protocolos HTTP, permitindo-nos focar em escrever nossa lógica de negócios.

## Veja Também:

1. [Documentação oficial do httpc](http://erlang.org/doc/man/httpc.html)

2. [Documentação de Gleam](https://gleam.run)