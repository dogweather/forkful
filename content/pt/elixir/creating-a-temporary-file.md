---
title:    "Elixir: Criando um arquivo temporário"
keywords: ["Elixir"]
---

{{< edit_this_page >}}

## Por que usar arquivos temporários em Elixir?

Criar arquivos temporários pode ser útil em muitos casos, como por exemplo, quando você precisa armazenar dados temporariamente antes de enviá-los para um banco de dados ou quando precisa de um espaço para armazenar arquivos temporários durante o processamento de uma aplicação. Em Elixir, há uma maneira simples e eficaz de criar arquivos temporários usando a biblioteca `File.Tempfile`.

## Como criar um arquivo temporário em Elixir

Podemos criar um arquivo temporário usando o módulo `File` juntamente com a função `Tempfile.open`. Dentro dela, podemos passar o nome do arquivo e as opções de criação. Por exemplo:

```Elixir
{:ok, file} = File.Tempfile.open("teste.txt")
```

Isso criará um arquivo temporário chamado "teste.txt" no diretório atual. Também podemos especificar um diretório diferente para a criação do arquivo:

```Elixir
{:ok, file} = File.Tempfile.open("teste.txt", dir: "/tmp")
```

Podemos até mesmo especificar o prefixo e o sufixo do arquivo temporário:

```Elixir
{:ok, file} = File.Tempfile.open("temp-", "", suffix: ".txt")
```

Este código criará um arquivo temporário com o nome "temp-<número aleatório>.txt" no diretório atual.

Além disso, podemos especificar as opções `:read`, `:write` e `:binary` para indicar que o arquivo deve ser aberto para leitura, escrita e em formato binário, respectivamente.

## Mergulho profundo

Ao criar arquivos temporários em Elixir, é importante lembrar que eles serão excluídos automaticamente pelo sistema após o uso. No entanto, se você quiser manter o arquivo após a execução do programa, pode usar a opção `:unlink` e definir seu valor como `:false`.

Além disso, podemos usar a função `File.chmod` para alterar as permissões do arquivo temporário antes de fechá-lo.

## Veja também

- [Documentação da biblioteca File (em inglês)](https://hexdocs.pm/elixir/File.html)
- [Mais informações sobre a criação de arquivos temporários (em inglês)](https://elixirforum.com/t/creating-temporary-files/7159)