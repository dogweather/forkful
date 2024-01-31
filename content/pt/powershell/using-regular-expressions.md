---
title:                "Utilizando expressões regulares"
date:                  2024-01-19
html_title:           "Bash: Utilizando expressões regulares"
simple_title:         "Utilizando expressões regulares"

category:             "PowerShell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/powershell/using-regular-expressions.md"
---

{{< edit_this_page >}}

## O Que & Porquê?
Regular expressions, ou regex, são padrões usados para encontrar correspondências em strings de texto. Programadores usam regex pela sua eficiência e flexibilidade na busca e manipulação de dados.

## Como Fazer:
```PowerShell
# Encontrando um código postal em um texto
$texto = "Meu CEP é 70040-020"
$regex = "\b\d{5}-\d{3}\b"
if ($texto -match $regex) {
    "Encontrado: " + $matches[0]
}

# Substituindo espaços duplicados por um único espaço
$texto_corrigido = "Este    texto tem espaços    extras"
$texto_corrigido -replace '\s+', ' '
```
Saída de exemplo:
```
Encontrado: 70040-020
Este texto tem espaços extras
```

## Mergulho Profundo
As expressões regulares têm suas raízes na teoria dos autômatos e linguagem formal, materializadas na década de 1950 pelo matemático americano Stephen Cole Kleene. Alternativas ao uso de regex incluem o processamento de strings com métodos tradicionais, embora muitas vezes sejam menos eficientes. No PowerShell, regex é implementado através do .NET, fornecendo uma variedade de operadores e classes para manipular texto.

## Veja Também
- [Tutorial interativo de regex](https://regexone.com/)
- [Testador de regex online](https://regexr.com/)
