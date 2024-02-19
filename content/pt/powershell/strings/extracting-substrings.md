---
aliases:
- /pt/powershell/extracting-substrings/
date: 2024-01-20 17:46:10.458274-07:00
description: "Extrair substrings significa selecionar partes espec\xEDficas de uma\
  \ string de texto. Programadores recorrem a esta t\xE9cnica para manipular e analisar\
  \ dados,\u2026"
lastmod: 2024-02-18 23:08:58.356067
model: gpt-4-1106-preview
summary: "Extrair substrings significa selecionar partes espec\xEDficas de uma string\
  \ de texto. Programadores recorrem a esta t\xE9cnica para manipular e analisar dados,\u2026"
title: Extraindo substrings
---

{{< edit_this_page >}}

## O Que é & Por Que?
Extrair substrings significa selecionar partes específicas de uma string de texto. Programadores recorrem a esta técnica para manipular e analisar dados, validar inputs, e trabalhar com informações de maneira mais eficiente.

## Como Fazer:
```PowerShell
# Exemplo de substring pelo indice e comprimento
$texto = "Olá, Mundo Programático!"
$substring = $texto.Substring(5,5)
$substring # Saída: Mundo

# Utilizando intervalos
$intervalo = 7..12
$substringRange = $texto[$intervalo]
$substringRange -join '' # Saída: Mundo

# Usando -match e $matches para extrair uma substring baseada em regex
if ($texto -match 'Mundo') {
    $matches[0] # Saída: Mundo
}
```

## Aprofundando:
Historicamente, a capacidade de manipular strings é fundamental desde o início da programação. No PowerShell, operações com substrings são simplificadas com métodos como `.Substring()`, o acesso por índices e o uso de expressões regulares (`-match` e a variável automática `$matches`). Alternativas incluem o split de strings com `.Split()`, substituições com `-replace`, ou usando classes do .NET para trabalhos mais complexos, como `System.Text.RegularExpressions.Regex`. A eficiência na utilização dessas ferramentas varia de acordo com o contexto, como tamanho da string e frequência da operação de extração em um script.

## Veja Também:
- [Guia sobre expressões regulares no .NET](https://docs.microsoft.com/pt-br/dotnet/standard/base-types/regular-expression-language-quick-reference)
- [StackOverflow: Dúvidas comuns de substrings em PowerShell](https://stackoverflow.com/questions/tagged/powershell+substring)
