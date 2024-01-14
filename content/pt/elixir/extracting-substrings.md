---
title:                "Elixir: Extraindo subtrings"
simple_title:         "Extraindo subtrings"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/elixir/extracting-substrings.md"
---

{{< edit_this_page >}}

## Por que extrair subcadeias?

Extrair subcadeias é uma tarefa importante ao trabalhar com strings em Elixir. Ao fazer isso, você pode separar partes específicas de uma string e manipulá-las individualmente. Isso é útil em muitos cenários, como formatação de dados, busca ou até mesmo análise de texto.

## Como fazer

Extrair substrings em Elixir é muito simples e pode ser feito de várias maneiras. Aqui estão dois exemplos que mostram como é possível obter subcadeias usando as funções `String.slice/2` e `String.contains?/2`:

```
Elixir

# Exemplo 1
string = "O Elixir é uma linguagem de programação funcional"
sub_string = String.slice(string, 2, 6)
IO.puts(sub_string) # Saída: lixir

# Exemplo 2
string = "O Elixir é uma linguagem de programação funcional"
sub_string = String.contains?(string, "funcional")
IO.puts(sub_string) # Saída: true
```

No primeiro exemplo, usamos a função `String.slice/2`, que recebe uma string, um índice inicial e um número de caracteres a serem extraídos a partir desse índice. Então, a saída será "lixir", que são os 6 caracteres começando do índice 2.

Já no segundo exemplo, usamos a função `String.contains?/2` para verificar se a string contém uma determinada subcadeia. No caso, queremos saber se a string contém a palavra "funcional", o que retorna `true`.

## Mergulho profundo

Além desses exemplos básicos, existem outras funções úteis para extrair subcadeias em Elixir. Algumas delas são:

- `String.split/2`: retorna uma lista de strings separadas por um caractere especificado
- `String.replace/4`: substitui uma subcadeia por outra em uma string
- `String.to_integer/1`: converte uma subcadeia em um número inteiro

É importante lembrar que as strings em Elixir são codificadas em UTF-8 e, por isso, certas funções podem se comportar de maneira diferente com caracteres fora do conjunto ASCII.

## Veja também

- [Documentação Elixir sobre strings](https://hexdocs.pm/elixir/String.html)
- [Cheatsheet de Elixir](https://devhints.io/elixir) para mais funções úteis de strings
- [Tutorial de Elixir para iniciantes](https://elixirschool.com/pt/) para aprender mais sobre a linguagem.