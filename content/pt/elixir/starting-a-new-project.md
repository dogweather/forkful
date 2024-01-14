---
title:                "Elixir: Começando um novo projeto"
programming_language: "Elixir"
category:             "Getting Started"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/elixir/starting-a-new-project.md"
---

{{< edit_this_page >}}

## Por que começar um novo projeto com Elixir?

Há muitas linguagens de programação por aí, então por que você deveria considerar utilizar Elixir para um novo projeto? Com uma sintaxe limpa e concisa, funcionalidade de concorrência incorporada e uma comunidade robusta de suporte, Elixir oferece uma experiência de desenvolvimento eficiente e confiável.

## Como começar um projeto em Elixir

Começar um novo projeto em Elixir é fácil e rápido. Primeiro, certifique-se de ter o Elixir instalado em sua máquina. Em seguida, crie um diretório para seu projeto e acesse-o por meio da linha de comando. Agora, vamos criar um arquivo `hello.exs` e adicionar o seguinte código dentro dele:

```Elixir
defmodule Hello do
  def print_greeting do
    IO.puts "Olá, mundo!"
  end
end
```

Podemos executar esse código utilizando o comando `elixir hello.exs` no diretório do projeto. O resultado será a impressão de "Olá, mundo!" no console.

## Aprofundando-se no início de um projeto

Ao começar um novo projeto, é importante pensar em como organizar suas pastas e arquivos para manter seu código limpo e estruturado. Além disso, você pode aproveitar as bibliotecas e frameworks disponíveis na comunidade Elixir para poupar tempo e esforços no desenvolvimento.

Outra vantagem de utilizar Elixir é sua funcionalidade de concorrência, por meio do mecanismo de processos leves (lightweight processes). Isso permite que seu código execute tarefas de forma paralela, melhorando a performance de sua aplicação.

Por fim, não esqueça de sempre consultar a documentação oficial e a comunidade em busca de ajuda e ideias para o seu projeto.

## Veja também

- [Site oficial do Elixir](https://elixir-lang.org/)
- [Documentação oficial do Elixir](https://elixir-lang.org/docs.html)
- [Elixir Forum, comunidade de discussão sobre Elixir](https://elixirforum.com/)
- [Elixir School, um guia gratuito de Elixir para iniciantes](https://elixirschool.com/pt/)