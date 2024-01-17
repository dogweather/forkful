---
title:                "Extraindo subcadeias"
html_title:           "PowerShell: Extraindo subcadeias"
simple_title:         "Extraindo subcadeias"
programming_language: "PowerShell"
category:             "PowerShell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/powershell/extracting-substrings.md"
---

{{< edit_this_page >}}

## O que & Porquê?
Extrair subcadeias de caracteres é um processo comum em programação que envolve a obtenção de partes específicas de uma string original. Isso pode ser útil em muitos casos, como por exemplo, se você precisar separar nomes e sobrenomes de uma lista de nomes ou extrair informações específicas de um texto. Os programadores geralmente fazem isso para tornar as strings mais legíveis e manipuláveis.

## Como fazer:
Vamos ver como extrair subcadeias de caracteres usando o PowerShell. Primeiro, vamos criar uma string original para trabalhar:
```PowerShell
$string = "Hello World"
```
Agora, usaremos o operador de indexação [ ] para extrair a subcadeia de caracteres desejada:
```PowerShell
$string[0..4]
```
Isso irá retornar "Hello", a subcadeia que começa no 1º caractere e vai até o 5º caractere (lembre-se que a contagem começa em 0 em PowerShell).

Outra forma de extrair subcadeias é usar o cmdlet ```Select-String```:
```PowerShell
$string | Select-String "World"
```
Isso irá retornar "World", a subcadeia que contém a palavra "World" na string original.

Ambos os métodos podem ser úteis em diferentes situações, então é importante conhecer ambas as abordagens.

## Deep Dive:
Extrair subcadeias de caracteres não é algo novo na programação. É amplamente usado em diferentes linguagens de programação e sempre foi considerado uma maneira eficaz de manipular strings. No entanto, em vez de usar o operador de indexação, algumas linguagens oferecem funções específicas para extrair subcadeias, o que pode ser mais fácil de usar ou oferecer mais recursos. No PowerShell, o cmdlet ```Select-String``` é uma dessas opções.

## Veja também:
- [Documentação do PowerShell](https://docs.microsoft.com/pt-br/powershell/)
- [Tutorial sobre manipulação de strings em PowerShell](https://sid-500.com/2018/05/16/powershell-do-you-know-how-to-use-strings-strings-and-more-strings/)