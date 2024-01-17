---
title:                "Unindo strings"
html_title:           "Elixir: Unindo strings"
simple_title:         "Unindo strings"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/elixir/concatenating-strings.md"
---

{{< edit_this_page >}}

## O que e por que?

Concatenar strings é o ato de juntar duas ou mais strings em uma só. Programadores fazem isso para criar novas strings com informações combinadas, facilitando a manipulação e exibição de dados.

## Como fazer:

```Elixir
#exemplo de concatenação de strings
nome = "João"
sobrenome = "Silva"

nome_completo = nome <> sobrenome # utilizamos o operador <> para concatenar as strings

IO.puts nome_completo # saída: JoãoSilva
```

## Mergulho Profundo:

Antes do surgimento de linguagens de programação mais modernas, concatenar strings era uma tarefa tediosa e complexa, muitas vezes envolvendo cálculos complicados para determinar a quantidade de espaço necessário para armazenar a nova string. No entanto, com o avanço da tecnologia, esse processo se tornou muito mais simples e eficiente.

Existem outras formas de combinar strings, como utilizando o operador `++` ou a função `String.concat/1`, mas o uso do operador `<>` é considerado o mais eficiente e mais utilizado na comunidade Elixir.

## Veja também:

- [Documentação oficial do operador <>](https://hexdocs.pm/elixir/operators.html#<>
- [Uso do operador <> em Elixir](https://medium.com/@Agesilao/o-operador-in-elixir-oadaf952d23)
- [Tutorial sobre concatenação de strings em Elixir](https://www.tutorialspoint.com/elixir/elixir_string_concatenation.htm)