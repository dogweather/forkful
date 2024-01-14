---
title:    "Elixir: Concatenando strings"
keywords: ["Elixir"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/pt/elixir/concatenating-strings.md"
---

{{< edit_this_page >}}

## Por que

A concatenação de strings é uma técnica importante em Elixir e pode ser utilizada para unir múltiplas strings em uma única, facilitando a manipulação e apresentação de dados. Além disso, é uma habilidade essencial para a construção de aplicações robustas e escaláveis.

## Como Fazer

Para realizar a concatenação de strings em Elixir, podemos utilizar o operador `<>` ou a função `String.concat/2`. Vejamos alguns exemplos práticos abaixo:

```Elixir
"Hello " <> "World"         #=> "Hello World"
String.concat("I", " love ", "Elixir")     #=> "I love Elixir"
```
Os dois métodos produzem o mesmo resultado, mas o operador `<>` é mais utilizado devido à sua simplicidade e facilidade de leitura.

Caso precisemos concatenar um grande número de strings, também podemos utilizar a função `Enum.join/2`, que percorre uma lista de strings e as concatena em uma única string. Veja o exemplo abaixo:

```Elixir
lista = ["Elixir", "é", "uma", "linguagem", "incível"]
Enum.join(lista, " ")       #=> "Elixir é uma linguagem incrível"
```

## Aprofundando-se

É importante lembrar que strings em Elixir são imutáveis, ou seja, uma vez criadas, elas não podem ser modificadas. Portanto, cada vez que utilizamos um método de concatenação, na verdade estamos criando uma nova string. Por esse motivo, devemos evitar concatenações excessivas em operações de alto desempenho.

Uma alternativa para a concatenação de strings pode ser o uso de templates, que permitem inserir valores dinâmicos em uma string sem a realocação de memória. Por exemplo:

```Elixir
nome = "Maria"
idade = 28
"Olá, meu nome é #{nome} e tenho #{idade} anos."   #=> "Olá, meu nome é Maria e tenho 28 anos."
```

## Veja Também

- [Documentação oficial do Elixir sobre strings](https://hexdocs.pm/elixir/String.html)
- [Tutorial sobre concatenação de strings em Elixir](https://www.tutorialspoint.com/elixir/elixir_strings.htm)
- [Discussão sobre o uso eficiente de string em Elixir](https://elixirforum.com/t/best-practices-for-string-manipulation/493/6)