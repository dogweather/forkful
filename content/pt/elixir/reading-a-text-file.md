---
title:                "Elixir: Lendo um arquivo de texto"
simple_title:         "Lendo um arquivo de texto"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/elixir/reading-a-text-file.md"
---

{{< edit_this_page >}}

## Por que ler um arquivo de texto em Elixir?

Ler um arquivo de texto é uma tarefa comum em programação e é importante conhecer as diferentes maneiras de realizar essa tarefa em cada linguagem. Neste artigo, vamos explorar como ler um arquivo de texto em Elixir e discutir os benefícios dessa habilidade na sua habilidade em programar.

## Como fazer:

Para ler um arquivo de texto em Elixir, podemos utilizar a função `File.read!/1`. Essa função recebe um caminho para o arquivo e retorna o conteúdo do arquivo como uma String. Podemos utilizar a sintaxe de pipeline para encadear essas operações:

```
Elixir
conteudo = File.read!("caminho/do/arquivo.txt")
|> String.upcase()

IO.puts(conteudo)
```

Nesse exemplo, lemos o arquivo e transformamos todo o seu conteúdo em letras maiúsculas antes de imprimir no console. A saída será o conteúdo do arquivo em letras maiúsculas.

## Mergulho Profundo:

Além de simplesmente ler o conteúdo de um arquivo, Elixir também possui funções para trabalhar com a própria estrutura do arquivo. Por exemplo, podemos utilizar `File.stream!/1` para criar um stream de dados a partir do arquivo, o que nos permite processar o arquivo de forma eficiente, especialmente se for um arquivo grande.

Também é possível utilizar o módulo `IO`, que fornece funções para ler o conteúdo do arquivo de forma mais robusta, lidando com possíveis erros ou interações com o sistema operacional.

## Veja também:

Aqui estão alguns recursos adicionais que podem ser úteis no seu aprendizado de como ler arquivos de texto em Elixir:

- [Documentação oficial da função `File.read!/1`](https://hexdocs.pm/elixir/File.html#read!/1)
- [Guia para manipulação de arquivos em Elixir](https://blog.appsignal.com/2018/03/27/manipulating-files-elixir.html)
- [Vídeo tutorial sobre como ler e escrever arquivos em Elixir](https://youtu.be/Zzt0RoARnTQ)

Agora que você sabe como ler um arquivo de texto em Elixir, pratique e explore outras funcionalidades relacionadas a arquivos na linguagem. Bom aprendizado!