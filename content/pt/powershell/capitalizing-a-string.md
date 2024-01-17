---
title:                "Maiúsculando uma string"
html_title:           "PowerShell: Maiúsculando uma string"
simple_title:         "Maiúsculando uma string"
programming_language: "PowerShell"
category:             "PowerShell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/powershell/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## O que & Porquê?
Capitalizar uma string é simplesmente torná-la toda em maiúsculas. Isso pode ser útil para padronizar a formatação de textos, facilitar a leitura e até mesmo para comparar strings. Programadores muitas vezes fazem isso quando precisam comparar duas strings para garantir que não haja diferenças entre maiúsculas e minúsculas.

## Como fazer:
```PowerShell
$string = "exemplo de string"
Write-Host "String original: $string"
Write-Host "String capitalizada: $($string.ToUpper())"
```
A saída será:
```
String original: exemplo de string
String capitalizada: EXEMPLO DE STRING
```
## Mergulho Profundo:
Capitalizar strings é uma técnica comum em programação, com variações em diferentes linguagens de programação. Em linguagens como C#, Java e Python, os programadores podem usar métodos específicos para capitalizar strings. No PowerShell, o método `ToUpper()` pode ser usado tanto em strings como em variáveis, tornando-a uma opção conveniente.

Outra opção é usar a função `PascalCase` do PowerShell, que capitalizará cada palavra em uma string, seguindo o padrão Pascal Case. Essa função também é útil para padronizar a formatação de textos e criar nomes de variáveis ou funções mais legíveis.

## Veja Também:
- [Método ToUpper() da documentação do PowerShell](https://docs.microsoft.com/en-us/powershell/module/microsoft.powershell.core/string-methods/toupper?view=powershell-7.1)
- [Função PascalCase do Powershell Gallery](https://www.powershellgallery.com/packages/PowerShellExtensions/1.0/Content/functions\PascalCase.ps1)