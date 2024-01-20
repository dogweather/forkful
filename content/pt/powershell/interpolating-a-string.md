---
title:                "Interpolando uma string"
html_title:           "Java: Interpolando uma string"
simple_title:         "Interpolando uma string"
programming_language: "PowerShell"
category:             "PowerShell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/powershell/interpolating-a-string.md"
---

{{< edit_this_page >}}

## O Que & Porquê?

A interpolação de strings é um processo de substituição de variables ou expressões embedidas dentro de strings por seus valores respectivos. Os programadores usam isso para tornar o código mais legível e também para melhor formatação e apresentação de mensagens de texto.

## Como Fazer:

Você pode começar a explorar a interpolação de strings no PowerShell com o código abaixo:

```PowerShell
$nome = "Pedro"
$idade = 23
"$nome tem $idade anos."
```

A saída seria:

```PowerShell
Pedro tem 23 anos.
```

Note que as variáveis `$nome` e `$idade` estão dentro das aspas duplas. Portanto, seus valores são interpolados na string.

## Imersão Profunda:

Historicamente, a interpolação de strings já existe há bastante tempo em muitas linguagens de programação, incluindo Perl, Ruby e, é claro, PowerShell, uma vez que melhora significativamente a legibilidade do código.

Existem alternativas à interpolação de strings. Por exemplo, a concatenação de strings:

```PowerShell
$nome = "Pedro"
$idade = 23
$nome + " tem " + $idade + " anos."
```

Contudo, a interpolação de strings tende a ser mais limpa e fácil de ler.

Informações importantes de implementação: no PowerShell, a interpolação de strings só ocorre com aspas duplas `" "`. As aspas simples `' '` tratam o conteúdo como string literal, nada é interpolado.

## Veja Também:

Para mais informações sobre a interpolação de strings no PowerShell, consulte os links abaixo:

1. [String Interpolation in PowerShell](https://ss64.com/ps/syntax-operators.html)

Lembre-se, a melhor maneira de aprender é praticando, então tente usar a interpolação de strings no seu próximo programa PowerShell.