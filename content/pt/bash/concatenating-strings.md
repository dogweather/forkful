---
title:                "Criando sequências de caracteres"
html_title:           "Bash: Criando sequências de caracteres"
simple_title:         "Criando sequências de caracteres"
programming_language: "Bash"
category:             "Bash"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/bash/concatenating-strings.md"
---

{{< edit_this_page >}}

## O que & Porquê?

Concatenar strings é o processo de combinar duas ou mais strings em uma única string. Programadores fazem isso para criar strings maiores e mais complexas que podem ser usadas em seus programas. Isso pode ser útil para criar mensagens de saída personalizadas, criar nomes de arquivos dinâmicos ou até mesmo formar URLs.

## Como fazer:

Para concatenar strings em Bash, basta usar o operador de concatenação ```$``` entre as strings que deseja combinar. Aqui está um exemplo:

```
nome="Maria"
sobrenome="Silva"
nome_completo="$nome $sobrenome"
echo "Meu nome completo é $nome_completo."
```

Isso irá gerar a seguinte saída:

```
Meu nome completo é Maria Silva.
```

## Mergulho profundo:

Concatenar strings em Bash é uma funcionalidade básica e útil, mas também pode ser feito de outras maneiras. Por exemplo, você também pode usar o comando ```cat``` para combinar o conteúdo de arquivos em uma única string. Além disso, é possível usar o operador de concatenação ```+``` se estiver usando a linguagem de script do Bash.

Para implementar a concatenação de strings em seu código Bash, é importante lembrar que é preciso usar o operador ```$``` e também inserir as strings entre aspas duplas para que sejam interpretadas corretamente.

## Veja também:

- [The Linux Command Line: Concatenating Strings in Bash](http://www.linuxjournal.com/content/concatenating-strings-bash)
- [Stack Overflow: Concatenate string in Bash](https://stackoverflow.com/questions/4181703/how-to-concatenate-string-variables-in-bash)