---
title:                "Elixir: Escrevendo um arquivo de texto"
simple_title:         "Escrevendo um arquivo de texto"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/elixir/writing-a-text-file.md"
---

{{< edit_this_page >}}

## Por que escrever um arquivo de texto em Elixir?

Escrever um arquivo de texto pode ser uma tarefa simples em qualquer linguagem de programação, mas em Elixir, pode oferecer uma série de vantagens. Ao escrever um arquivo de texto, você pode armazenar informações importantes, como resultados de cálculos ou dados pessoais, de forma organizada e acessível. Além disso, escrever um arquivo de texto em Elixir pode ser uma ótima maneira de praticar e aprimorar suas habilidades de programação nessa linguagem.

## Como fazer um arquivo de texto em Elixir

Para começar a escrever um arquivo de texto em Elixir, você precisará usar a função `File.write/2`. Essa função recebe dois argumentos: o caminho do arquivo que deseja criar e o conteúdo que deseja escrever no arquivo. Por exemplo:

```Elixir
File.write("arquivo.txt", "Olá mundo!")
```

Nesse exemplo, criamos um arquivo chamado "arquivo.txt" e escrevemos a mensagem "Olá mundo!" dentro dele. Você também pode usar o operador de concatenação `<>` para adicionar conteúdo ao seu arquivo. Por exemplo:

```Elixir
File.write("arquivo.txt", "Olá" <> " " <> "mundo!")
```

Isso produzirá o mesmo resultado do exemplo anterior.

Para ler o conteúdo de um arquivo de texto que você criou, basta usar a função `File.read/1`. Essa função recebe o caminho do arquivo como argumento e retorna o conteúdo do arquivo como uma string.

```Elixir
File.read("arquivo.txt") # retorna "Olá mundo!"
```

## Mergulho Profundo

Além das funções `File.write/2` e `File.read/1`, existem outras funções úteis para trabalhar com arquivos de texto em Elixir. Por exemplo, você pode usar a função `File.append/2` para adicionar conteúdo a um arquivo existente sem substituir o conteúdo já existente. Também é possível criar novos diretórios com a função `File.mkdir/1` e listar os arquivos e diretórios em um determinado diretório com a função `File.ls/1`.

Para mais informações sobre como trabalhar com arquivos em Elixir, consulte a documentação oficial da linguagem. E lembre-se sempre de fechar o arquivo após terminar de usá-lo com a função `File.close/1`, para evitar problemas de desempenho.

## Veja também

- [Documentação oficial do Elixir sobre manipulação de arquivos](https://hexdocs.pm/elixir/File.html)
- [Tutorial sobre como escrever e ler arquivos em Elixir](https://thoughtbot.com/blog/reading-and-writing-files-in-elixir)