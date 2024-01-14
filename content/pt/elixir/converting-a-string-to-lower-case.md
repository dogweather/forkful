---
title:    "Elixir: Convertendo uma string para minúsculas"
keywords: ["Elixir"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/pt/elixir/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## Por que

Há várias razões pelas quais alguém pode querer converter uma string para letras minúsculas em Elixir. Isso pode ser útil para padronizar a formatação de dados ou para comparar strings de maneira mais precisa.

## Como Fazer

A conversão de uma string para minúsculas em Elixir é bastante simples e pode ser feita de diferentes maneiras. A primeira opção é utilizar a função `String.downcase/1` que recebe uma string como argumento e retorna uma nova string com todas as letras em minúsculo. Veja o exemplo abaixo:

```Elixir
string = "Olá, Elixir"
String.downcase(string)
```

A saída desse código será `"olá, elixir"`. Além disso, também é possível utilizar o operador de pipe `|>` para encadear essa função com outras operações. Por exemplo:

```Elixir
string = "Olá, Elixir"
string |> String.downcase() |> String.trim()
```

Nesse caso, a saída será `"olá, elixir"` pois a função `String.trim/1` é utilizada para remover espaços em branco no início e no final da string.

## Deep Dive

Ao utilizar a função `String.downcase/1`, é importante observar que ela apenas converte letras ASCII para minúsculas. Ou seja, caracteres especiais como acentos não serão convertidos. Caso seja necessário, é possível utilizar a função `String.downcase_utf8/1` que lida com caracteres Unicode. Além disso, é importante lembrar que as strings em Elixir são imutáveis, ou seja, sempre que uma alteração é feita, uma nova string é criada em memória, portanto é importante considerar o impacto dessa função em termos de performance em casos de manipulação de grandes volumes de dados.

## Veja Também

- [Documentação oficial de Strings](https://hexdocs.pm/elixir/String.html)
- [Como manipular strings em Elixir](https://www.lalala.com.br/como-manipular-strings-em-elixir)
- [Tutorial de Elixir do iniciante ao avançado](https://www.lalala.com.br/tutorial-elixir-iniciante-avancado)