---
title:                "Criando um arquivo temporário"
html_title:           "Bash: Criando um arquivo temporário"
simple_title:         "Criando um arquivo temporário"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/elixir/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## O quê e Por quê?
Criar um arquivo temporário é o processo de definir um arquivo que será usado para armazenar informações temporariamente. Os programadores fazem isso quando precisam lidar com grandes quantidades de dados que não precisam ser armazenados a longo prazo.

## Como Fazer?
Aqui está um exemplo no Elixir de como criar um arquivo temporário usando a biblioteca 'Temp':

```Elixir
defmodule TempFile do
  require File

  def create_temp do
    {:ok, path} = Temp.path()
    File.write!(path, "content")
    IO.puts("Arquivo temporário criado em: #{path}")
  end
end

TempFile.create_temp()
```
A execução deste código resulta em uma saída semelhante a:

```Elixir
Arquivo temporário criado em: /tmp/1jzi04410323
```

## Mergulho Profundo

A prática de criar arquivos temporários é algo que existe desde os primeiros dias da programação. Naquele tempo, a memória era escassa, então era comum escrever dados em um arquivo temporário.

Uma alternativa para a criação de arquivos temporários seria usar um banco de dados em memória, como o Redis. No entanto, isso pode não ser prático para alguns casos de uso devido aos custos de desempenho.

A implementação exata do processo de criação de um arquivo temporário pode variar dependendo da biblioteca que você está usando. Na biblioteca 'Temp' do Elixir, um identificador único é gerado e um arquivo com esse nome é criado no diretório temporário.

## Veja Também

- [Documentação Oficial do Elixir](https://hexdocs.pm/elixir/File.html)
- [Temp](https://hexdocs.pm/temp/readme.html): uma biblioteca para criação de arquivos e diretórios temporários em Elixir.
- [Redis](https://redis.io/): Um banco de dados em memória utilizado como alternativa à criação de arquivos temporários.