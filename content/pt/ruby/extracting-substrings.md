---
title:                "Ruby: Extraindo subtrings"
simple_title:         "Extraindo subtrings"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/ruby/extracting-substrings.md"
---

{{< edit_this_page >}}

## Por que

Um dos aspectos interessantes da programação é a capacidade de manipular strings para realizar várias tarefas. Uma das tarefas mais comuns é extrair uma parte específica de uma string, também conhecida como substring. Neste artigo, vamos explorar como fazer isso usando Ruby.

## Como Fazer

Extrair substrings em Ruby é bastante fácil. Basta usar o método `[]` em uma string e especificar o índice inicial e final da parte que você deseja extrair. Por exemplo:

```Ruby
my_string = "Oi tudo bem?"
puts my_string[3..6] # saída: tudo
```

O código acima criará uma string chamada `my_string` e em seguida, usará o método `[]` para extrair a substring "tudo" que está entre os índices 3 e 6 (incluindo o caractere do índice 6).

Agora, vamos dar uma olhada em um exemplo mais complexo. Digamos que temos uma string com vários nomes separados por vírgulas e queremos extrair o terceiro nome. Podemos fazer isso usando o método `split` para separar a string em um array e, em seguida, usar o método `[]` para extrair o terceiro elemento do array. Veja o código abaixo:

```Ruby
names = "Maria, João, Pedro, Ana, Carla"
array_names = names.split(", ") # dividindo a string em um array
puts array_names[2] # saída: Pedro
```

Agora, você pode estar se perguntando por que usamos `[2]` em vez de `[3]` para obter o terceiro elemento. Lembre-se que em Ruby, os índices começam em 0, então `array_names[2]` corresponde ao terceiro elemento do array.

## Deep Dive

Agora que já vimos como extrair substrings em Ruby, vamos falar um pouco mais sobre como essa técnica funciona. Internamente, o método `[]` usa o operador de busca `[]` da classe `String`, que aceita um índice inicial e, opcionalmente, um comprimento. Isso significa que além de especificar o índice inicial e final, você também pode especificar um comprimento para a substring que deseja extrair.

Além disso, vale ressaltar que o método `[]` não modifica a string original, ele apenas retorna uma nova string com a parte extraída. Se você quiser modificar a string original, pode usar o método `[]=` que aceita os mesmos argumentos que o método `[]`e modifica a string original.

## Veja Também

- [Documentação oficial do método `[]` da classe `String`](https://ruby-doc.org/core-3.0.0/String.html#method-i-5B-5D)
- [Tutorial sobre strings em Ruby](https://www.rubyguides.com/ruby-string/)
- [Exemplos de código práticos com strings em Ruby](https://www.sitepoint.com/ruby-string-manipulation/#:~:text=Ruby%20String%20Manipulation&text=Ruby%20has%20great%20string%20manipulation,fine%2Dtuned%20for%20string%20manipulation.)