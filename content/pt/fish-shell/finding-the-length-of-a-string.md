---
title:                "Descobrindo o comprimento de uma string"
date:                  2024-01-20T17:47:26.054129-07:00
model:                 gpt-4-1106-preview
simple_title:         "Descobrindo o comprimento de uma string"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/fish-shell/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## O Que & Porquê?
Descobrir o comprimento de uma string é saber quantos caracteres ela tem. Programadores fazem isso para validar dados, limitar input, ou otimizar espaços de armazenamento.

## Como Fazer:
```Fish Shell
# Para obter o comprimento de uma string:
set string "olá, mundo"
echo $string | wc -m
```
Output:
```
11
```
(Note que o output conta também o espaço e a quebra de linha.)

```Fish Shell
# Alternativamente, use string length para um resultado mais preciso:
set string "olá, mundo"
string length -- $string
```
Output:
```
10
```

## Mergulho Profundo
Historicamente, o Fish Shell sempre se pautou pela facilidade de uso e pela sintaxe amigável. Contar caracteres em strings é uma funcionalidade essencial desde os primórdios da programação. No entanto, os métodos podem variar. Por exemplo, em algumas linguagens, você conta os caracteres diretamente com uma função built-in, enquanto no Fish (como no Unix em geral), freqüentemente se utiliza uma combinação de comandos como 'echo' e 'wc'.

O comando `string length` é a maneira nativa do Fish para obter o comprimento de uma string, e geralmente é preferível sobre o uso de pipes e comandos externos pois é mais limpo e eficiente. Além do mais, evita as armadilhas de contar caracteres invisíveis ou espaços em branco adicionais.

A implementação do `string length` é otimizada para operar com a codificação de caracteres do Fish Shell, garantindo que você obtenha uma contagem precisa independentemente do conteúdo da string (uma vantagem sobre o uso de `wc -m`, que pode se confundir com caracteres multibyte, dependendo do locale).

## Veja Também
- Documentação oficial do comando `string` no Fish: https://fishshell.com/docs/current/cmds/string.html
- Tutorial Fish Shell para iniciantes: https://fishshell.com/docs/current/tutorial.html
- Página de manual do comando `wc`: https://man7.org/linux/man-pages/man1/wc.1.html