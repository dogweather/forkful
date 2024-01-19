---
title:                "Baixando uma página da web"
html_title:           "Bash: Baixando uma página da web"
simple_title:         "Baixando uma página da web"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/elixir/downloading-a-web-page.md"
---

{{< edit_this_page >}}

# Baixar uma página da web com Elixir

## O que e Por quê?
Baixar uma página da web é o processo de obter o conteúdo de HTML de um site. Programadores fazem isso por muitas razões, como analisar dados, testar a velocidade do servidor ou verificar atualizações em tempo real.

## Como Fazer:
Utilizamos a biblioteca `HTTPoison` do Elixir para este propósito. Se ainda não a instalou, utilize o seguinte comando: `mix deps.get httpoison`.

```Elixir
defmodule BaixarPagina do
 require HTTPoison
 def baixar(url) do
   case HTTPoison.get(url) do
     {:ok, response} ->
       IO.puts "Baixado com sucesso:\n #{response.body}"
     {:error, reason} ->
       IO.puts "Falha ao baixar a página. Razão: #{reason}"
   end
 end
end
```
Chamada:
```Elixir
BaixarPagina.baixar("https://elixir-lang.org/")
```

## Mergulho Profundo
1. Histórico: Elixir foi criado em 2011 por José Valim, proporcionando ergonomia para a utilização da máquina virtual Erlang.
2. Alternativas: Outras bibliotecas, como `Tesla` e `Mojito`, também podem ser usadas para baixar páginas da web em Elixir.
3. Detalhes da Implementação: A chamada `HTTPoison.get(url)` realiza uma solicitação GET HTTP à URL fornecida e retorna um conjunto de duas tuplas, `{:ok, response}` em caso de sucesso e `{:error, reason}` para falhas.

## Veja Também
1. [HTTPoison no GitHub](https://github.com/edgurgel/httpoison)
2. [Tutorial completo do Elixir](https://elixirschool.com/en/)
3. [Documentação oficial do Elixir](https://elixir-lang.org/docs.html)
4. [Tesla no Hexdocs](https://hexdocs.pm/tesla/readme.html)
5. [Mojito no Hexdocs](https://hexdocs.pm/mojito/readme.html)