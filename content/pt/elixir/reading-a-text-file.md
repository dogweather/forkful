---
title:    "Elixir: Lendo um arquivo de texto"
keywords: ["Elixir"]
---

{{< edit_this_page >}}

## Por que ler um arquivo de texto?

Ler arquivos de texto é uma tarefa comum em muitas aplicações de programação. Ao ler um arquivo, você pode acessar seu conteúdo e usá-lo nos seus códigos. Isso pode ser útil ao lidar com grandes quantidades de dados ou quando você precisa armazenar informações de forma persistente para uso posterior.

## Como ler um arquivo com Elixir

Para ler um arquivo de texto em Elixir, podemos usar a função `File.read/1`. Esta função recebe como argumento o caminho completo do arquivo que desejamos ler e retorna uma tupla contendo o status da operação e o conteúdo do arquivo.

```Elixir
# Exemplo de leitura de um arquivo chamado "exemplo.txt"
{:ok, file_content} = File.read("path/to/file/exemplo.txt")
```

Podemos então acessar o conteúdo do arquivo utilizando a variável `file_content` e realizar qualquer operação desejada com ele.

## Mais sobre a leitura de arquivos

Ao ler um arquivo de texto, é importante considerar que o conteúdo será retornado como uma string em UTF-8, independentemente da codificação original do arquivo. Isso pode ser uma vantagem em termos de consistência, mas pode ser um problema se o arquivo contiver caracteres de outra codificação.

Para evitar problemas de codificação, é possível usar a função `File.read!/1` que levanta uma exceção caso haja algum erro na leitura do arquivo. Além disso, também é importante fechar o arquivo após ler seu conteúdo. Isso pode ser feito utilizando a função `File.close/1` após o uso da variável `file_content`.

## Veja também

- [Documentação oficial do Elixir sobre File](https://hexdocs.pm/elixir/File.html)
- [Leitura de arquivos de texto em Elixir