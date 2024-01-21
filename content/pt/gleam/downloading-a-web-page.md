---
title:                "Baixando uma página da web"
date:                  2024-01-20T17:44:19.516044-07:00
model:                 gpt-4-1106-preview
simple_title:         "Baixando uma página da web"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/gleam/downloading-a-web-page.md"
---

{{< edit_this_page >}}

## O Quê & Por Quê?
Baixar uma página da web é pegar o conteúdo dela através da internet e trazê-lo para o nosso programa. Fazemos isso para analisar, verificar o status ou simplesmente armazenar dados de páginas web de interesse.

## How to:
Vamos usar a biblioteca `gleam_http` para pegar o conteúdo de uma página:

```gleam
import gleam/http
import gleam/httpc

pub fn download_page(url: String) -> Result(String, Nil) {
  httpc.get(url)
  |> result.map(http.Response.body) 
}
```

Exemplo de uso e saída:

```gleam
pub fn main() {
  let url = "http://example.com"
  case download_page(url) {
    Ok(body) -> 
      io.println(body)
    Error(_) ->
      io.println("Falha ao baixar a página.")
  }
}
```

## Deep Dive
Historicamente, buscar páginas da web é uma necessidade comum, muito antes do Gleam existir; linguagens como Python e JavaScript têm feito isso há anos. Gleam, sendo fortemente inspirado por Erlang, proporciona robustez e concorrência para essa tarefa com vantagens tipo segurança adicional e imutabilidade de dados.

Alternativas para fazer download de uma página no Gleam incluem construir requisições manualmente usando módulos como `gleam/otp/http` ou até aproveitar os poderes do Erlang diretamente com `:httpc`.

Quando a `gleam_httpc.get` é chamada, ela faz uma requisição GET HTTP na URL fornecida e retorna o corpo da resposta em caso de sucesso. Importante observar que erros de rede ou status HTTP diferentes de 200 irão resultar em um resultado `Error`.

## See Also
- Documentação oficial do Gleam para trabalhar com HTTP: [https://hexdocs.pm/gleam_http/](https://hexdocs.pm/gleam_http/)
- Repositório do `gleam_http` no GitHub para contribuições ou referência: [https://github.com/gleam-lang/http](https://github.com/gleam-lang/http)