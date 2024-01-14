---
title:    "Elixir: Lendo um arquivo de texto"
keywords: ["Elixir"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/pt/elixir/reading-a-text-file.md"
---

{{< edit_this_page >}}

## Por que ler um arquivo de texto em Elixir?

Ler e manipular arquivos de texto é uma tarefa comum em muitas linguagens de programação, incluindo Elixir. Saber como ler um arquivo de texto pode ser útil para acessar informações salvas em arquivos e processá-las em seu código. Neste artigo, vamos explorar os conceitos básicos de leitura de arquivos de texto em Elixir e como você pode incorporar isso em seus projetos.

## Como ler um arquivo de texto em Elixir

Em Elixir, a leitura de um arquivo de texto pode ser feita usando a função `File.read/2`, que aceita dois argumentos: o caminho do arquivo que você deseja ler e o modo de leitura (por exemplo, `:line` para ler o arquivo linha por linha). Veja um exemplo de como ler um arquivo de texto em Elixir:

```elixir
content = File.read("arquivo.txt")
IO.puts(content)
```

Neste exemplo, a função `IO.puts` é usada para imprimir o conteúdo do arquivo no console. Você também pode usar a função `File.read!/2` para ler o arquivo diretamente como uma string, sem a necessidade de chamar `IO.puts` para imprimi-lo. Confira a documentação oficial do Elixir para aprender mais sobre as diferentes opções de modo de leitura e como manipular o conteúdo do arquivo.

## Mergulho profundo

Além da função `File.read/2`, existem outras opções disponíveis em Elixir para ler arquivos de texto. Por exemplo, a biblioteca `File` possui a função `stream!/2` que retorna um fluxo de dados do arquivo, permitindo o processamento de grandes arquivos de forma eficiente. Além disso, a biblioteca `File` também oferece funções para escrever e manipular arquivos, tornando-a uma ferramenta útil para gerenciar dados em seu código.

## Veja também

- Documentação do Elixir sobre leitura de arquivos: https://hexdocs.pm/elixir/File.html
- Exemplo de leitura de arquivo no site oficial do Elixir: https://elixir-lang.org/getting-started/introduction.html#file-io