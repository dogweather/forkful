---
title:                "Bash: Encontrando o comprimento de uma string"
programming_language: "Bash"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/bash/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

# Porque encontrar o comprimento de uma string no Bash

Encontrar o comprimento de uma string é uma das tarefas básicas em programação, especialmente ao trabalhar com o Bash. Saber o comprimento de uma string pode ser útil para diferentes propósitos, desde validar entradas do usuário a manipular e formatar dados. Neste post, vamos mostrar como você pode encontrar o comprimento de uma string utilizando o Bash.

## Como fazer

Para encontrar o comprimento de uma string no Bash, você pode usar o comando `expr length`, seguido da string entre aspas. Por exemplo:

```Bash
expr length "Hello world"
```

A saída seria `11`, pois a string "Hello world" possui 11 caracteres. Você também pode atribuir o resultado a uma variável, por exemplo:

```Bash
length=$(expr length "Hello world")
echo $length
```

A saída seria novamente `11`. Outra opção é utilizar o operador `${#}`, seguido da string entre chaves. Por exemplo:

```Bash
echo ${#"Hello world"}
```

A saída seria novamente `11`. Agora que você sabe como encontrar o comprimento de uma string, vamos ver alguns exemplos de uso em diferentes situações.

## Profundando

Encontrar o comprimento de uma string é apenas o primeiro passo. Você também pode usar esse conhecimento para realizar outras tarefas no Bash. Por exemplo, você pode extrair um trecho específico de uma string utilizando o operador `${:}`, seguido do índice inicial e do comprimento desejado. Veja este exemplo:

```Bash
string="Hello world"
echo ${string:3:5}
```

A saída seria `lo wo`, pois o comando está pedindo para extrair a partir do índice 3 um trecho com 5 caracteres. Além disso, você também pode usar o comando `wc` para contar o número de palavras em uma string, como mostrado abaixo:

```Bash
string="Hello world"
wc -w <<< $string
```

A saída seria `2`, pois a string possui duas palavras. Existem diversas possibilidades quando se trata de manipular strings no Bash, e conhecer o comprimento dela é essencial para realizar essas tarefas.

## Veja também
- [Documentação oficial do Bash](https://www.gnu.org/software/bash/)
- [Tutorial de manipulação de strings no Bash](https://www.tutorialspoint.com/unix/unix-regular-expressions.htm)
- [Referência rápida de comandos do Bash](https://devhints.io/bash)