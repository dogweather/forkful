---
title:                "Extraindo subseqüências"
html_title:           "Fish Shell: Extraindo subseqüências"
simple_title:         "Extraindo subseqüências"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/fish-shell/extracting-substrings.md"
---

{{< edit_this_page >}}

## Por que extrair substrings?

Extrair substrings é uma tarefa comum na programação, especialmente quando se trabalha com texto. Isso pode ser útil para filtrar informações específicas de uma grande string, como extrair um nome de usuário de um endereço de e-mail ou obter a data de um texto mais longo. A capacidade de extrair substrings também pode facilitar tarefas de manipulação de dados e formatação de textos.

## Como fazer isso com o Fish Shell

O Fish Shell possui uma função interna chamada `string sub`, que permite a extração de substrings com base em um padrão específico. Veja alguns exemplos abaixo.

```Fish Shell
# Extrair o primeiro nome de um endereço de e-mail
set email "joao.silva@gmail.com"
echo $email | string sub -r "(\\w+)\\.\\w+@.+\\.(\\w+)" --repl '$1'
# Output: joao

# Extrair a data de uma string no formato DD/MM/AAAA
set data "Hoje é dia 10/07/2021"
echo $data | string sub -r "(\\d{2}/\\d{2}/\\d{4})" --repl '$1'
# Output: 10/07/2021
```

## Mais informações e exemplos

A função `string sub` pode ser usada de várias maneiras para extrair substrings de maneira eficiente. Além do padrão e da substituição, é possível especificar opções adicionais, como ignorar letras maiúsculas e minúsculas ou extrair todas as ocorrências em uma string.

Um exemplo para extrair todas as ocorrências de uma palavra específica em um texto:

```Fish Shell
set texto "O rato roeu a roupa do rei de Roma"
echo $texto | string sub -a -r "(r\\w+)" --repl '$1'
# Output: rato roeu roupa rei Roma
```

Para obter mais informações e exemplos sobre a função `string sub`, consulte a documentação do Fish Shell.

## Veja também

- [Documentação da função `string sub` no Fish Shell](https://fishshell.com/docs/current/#string-sub)
- [Tutorial sobre manipulação de strings com o Fish Shell](https://dev.to/benjamingroberts/fish-shell-how-to-manipulate-strings-2jk8)