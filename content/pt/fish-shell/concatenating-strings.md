---
title:                "Concatenando strings"
html_title:           "Fish Shell: Concatenando strings"
simple_title:         "Concatenando strings"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/fish-shell/concatenating-strings.md"
---

{{< edit_this_page >}}

## Por que concatenar strings?

Você pode estar se perguntando por que você deveria se preocupar com a concatenação de strings. Afinal, por que escrever mais código quando você pode simplesmente escrever todo o texto junto? Mas a concatenação de strings pode ser útil quando você está trabalhando com variáveis e precisa combinar diferentes valores em uma única string.

## Como fazer

Agora que entendemos por que a concatenação de strings pode ser útil, vamos aprender como fazer isso usando o Fish Shell. Existem várias maneiras de concatenar strings nesta linguagem de shell, mas uma das formas mais comuns é usar o operador `+`.

```
Fish Shell

# Define duas variáveis com strings
set texto1 "Meu nome é "
set texto2 "Maria"

# Concatena as duas strings usando o operador "+"
set nome_completo $texto1 + $texto2

# Imprime o resultado
echo $nome_completo

# Output: Meu nome é Maria
```

Neste exemplo, definimos duas variáveis com as strings "Meu nome é" e "Maria". Em seguida, usamos o operador `+` para concatená-las em uma única variável chamada `nome_completo`. Por fim, imprimimos o resultado usando o comando `echo`.

Outra forma de concatenar strings é usando a função `string join`, que permite unir várias strings em uma única string.

```
Fish Shell

# Define duas variáveis com strings
set lista1 "Item 1" "Item 2" "Item 3"
set lista2 "Item 4" "Item 5" "Item 6"

# Concatena as duas listas em uma única string
set lista_completa (string join " " $lista1 $lista2)

# Imprime o resultado
echo $lista_completa

# Output: Item 1 Item 2 Item 3 Item 4 Item 5 Item 6
```

Neste exemplo, definimos duas variáveis com listas de strings e, em seguida, usamos a função `string join` para unir todas as strings em uma única lista. Podemos especificar um delimitador entre as strings, que neste caso é um espaço em branco.

## Explorando mais

Existem muitas outras formas de concatenar strings no Fish Shell, como usar o comando `string insert` para inserir uma string em uma posição específica de outra string, ou a função `string split` para dividir uma string em várias strings separadas por um delimitador. Se você quiser se aprofundar mais nesse assunto, consulte a documentação oficial do Fish Shell ou pesquise por tutoriais e exemplos online.

## Veja também

Se você quiser aprender mais sobre o Fish Shell ou se aprofundar em outros tópicos relacionados, confira os links abaixo:

- [Documentação oficial do Fish Shell](https://fishshell.com/docs/current/index.html)
- [Tutorial do Fish Shell no Medium](https://medium.com/@amarjeetkapoor04/getting-started-with-fish-shell-c49c5b57ace)
- [Mais exemplos de concatenar strings em scripts do Fish Shell](https://github.com/fish-shell/fish-shell/blob/master/share/functions/string.joins)