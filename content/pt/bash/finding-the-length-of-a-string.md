---
title:                "Bash: Encontrando o comprimento de uma string."
simple_title:         "Encontrando o comprimento de uma string."
programming_language: "Bash"
category:             "Bash"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/bash/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## Por que
Muitas vezes, ao programar em Bash, precisamos lidar com strings e saber o tamanho delas é essencial para manipular corretamente esses dados.

## Como
Para encontrar o tamanho de uma string em Bash, podemos usar o operador de substituição de parâmetro `${#string}`. Vamos ver um exemplo:

```
# Definir uma string
frase="Olá, mundo!"

# Encontrar o tamanho da string
tamanho=${#frase}

# Imprimir o resultado
echo "O tamanho da string é $tamanho"
```

O resultado será:

```
O tamanho da string é 12
```

## Deep Dive
O operador `${#string}` retorna o comprimento da string em caracteres. Mas é importante notar que ele conta todos os caracteres, incluindo espaços em branco e caracteres especiais. Além disso, ele também pode ser usado para encontrar o tamanho de uma variável ou de um parâmetro.

Também é possível usar o operador `${#string}` dentro de um loop para encontrar o tamanho de várias strings em um array. Por exemplo:

```
# Definir um array de strings
frases=("O mundo é um livro e aqueles que não viajam leem apenas uma página." "No meio do inverno, finalmente aprendi que havia em mim um verão invencível.")

# Percorrer o array e imprimir o tamanho de cada string
for frase in "${frases[@]}"
do
    echo "O tamanho da string '$frase' é ${#frase}"
done
```

O resultado será:

```
O tamanho de 'O mundo é um livro e aqueles que não viajam leem apenas uma página.' é 63
O tamanho de 'No meio do inverno, finalmente aprendi que havia em mim um verão invencível.' é 66
```

## Veja também
- [Bash Guide for Beginners](https://tldp.org/LDP/Bash-Beginners-Guide/html/)
- [Bash Scripting Tutorial](https://www.shellscript.sh/index.html)