---
title:                "Excluindo caracteres que correspondem a um padrão"
html_title:           "Fish Shell: Excluindo caracteres que correspondem a um padrão"
simple_title:         "Excluindo caracteres que correspondem a um padrão"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/fish-shell/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## O que & Por quê?
Deletar caracteres que correspondem a um padrão é quando removemos caracteres específicos de um texto ou string usando um padrão de pesquisa. Programadores geralmente fazem isso para limpar ou manipular dados de forma automatizada, economizando tempo e esforço.

## Como fazer:
Exemplos de código e saída de amostra dentro de blocos de código ```Fish Shell...```
```
# Exemplo 1:

# Texto original:
Olá, mundo! Este é um exemplo de texto.

# Comando para deletar caracteres que correspondem ao padrão 'e':
echo "Olá, mundo! Este é um exemplo de texto." | sed 's/e//g'

# Saída:
Olá, mundo! s é um xmplo d txt.

# Exemplo 2:

# Texto original:
12345abcde

# Comando para deletar todos os números:
echo "12345abcde" | tr -d [:digit:]

# Saída:
abcde
```

## Mergulho Profundo:
Deletar caracteres que correspondem a um padrão é uma técnica amplamente utilizada na programação, especialmente em linguagens de script como o Fish Shell. Existem outras alternativas para realizar essa tarefa, como utilizar expressões regulares ou funções específicas de manipulação de strings em outras linguagens. A implementação desse processo envolve um algoritmo de busca que identifica e remove os caracteres correspondentes, tornando-o um processo eficiente e flexível.

## Veja também:
- [Documentação oficial do Fish Shell](https://fishshell.com/docs/current/)
- [Tutorial de expressões regulares com o Fish Shell](https://www.digitalocean.com/community/tutorials/fish-shell-regular-expressions-pt)
- [Artigo sobre manipulação de strings em diferentes linguagens de programação](https://www.guru99.com/string-manipulation-in-c.html)