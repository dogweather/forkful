---
title:                "Convertendo uma string para minúsculas."
html_title:           "Bash: Convertendo uma string para minúsculas."
simple_title:         "Convertendo uma string para minúsculas."
programming_language: "Bash"
category:             "Bash"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/bash/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## Por que alguém se interessaria em converter uma string para minúsculas?

Se você está trabalhando com programação Bash, pode ser necessário converter uma string para minúsculas em algum momento. Isso pode ser útil para padronizar a entrada de dados ou para comparar strings sem diferenciar entre maiúsculas e minúsculas. Felizmente, existem maneiras simples de fazer isso com algumas linhas de código.

## Como fazer a conversão em Bash

Para converter uma string para minúsculas em Bash, você pode usar o comando "tr" ou a substituição de parâmetros do Bash.

```
# Usando o comando "tr"
texto="Olá MUNDO"
echo $texto | tr '[:upper:]' '[:lower:]' # saída: olá mundo

# Usando substituição de parâmetros
texto="EStE é UM TEXTO"
echo ${texto,,} # saída: este é um texto
```

## Mergulho profundo

Ao converter uma string para minúsculas em Bash, é importante entender o que está acontecendo por trás das cenas. O comando "tr" usa conjuntos de caracteres para transformar as letras maiúsculas em minúsculas. Já a substituição de parâmetros usa uma variável de expansão para converter a string em minúsculas. É sempre bom ter uma compreensão mais profunda desses conceitos ao trabalhar com programação.

## Veja também

- Documentação oficial do Bash: https://www.gnu.org/software/bash/
- Tutorial de Bash no Codecademy: https://www.codecademy.com/learn/learn-bash
- Guia de referência rápida de Bash: https://devhints.io/bash