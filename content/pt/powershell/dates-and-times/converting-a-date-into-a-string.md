---
date: 2024-01-20 17:37:27.368601-07:00
description: "Converter uma data em uma string \xE9 o processo de transforma\xE7\xE3\
  o de uma estrutura de data/hora em texto leg\xEDvel. Programadores fazem isso para\
  \ exibir datas\u2026"
lastmod: '2024-03-13T22:44:46.808345-06:00'
model: gpt-4-1106-preview
summary: "Converter uma data em uma string \xE9 o processo de transforma\xE7\xE3o\
  \ de uma estrutura de data/hora em texto leg\xEDvel."
title: Convertendo uma data em uma string
weight: 28
---

## Como Fazer:
Vamos direto ao ponto. No PowerShell, utilizamos o método `ToString()` para converter uma data em string, e podemos especificar o formato que queremos.

```PowerShell
# Data atual
$dataAtual = Get-Date

# Conversão padrão para string
$dataString = $dataAtual.ToString()
Write-Output $dataString

# Especificando formato
$dataFormatada = $dataAtual.ToString("dd/MM/yyyy")
Write-Output $dataFormatada

# Usando formatos culturais
[CultureInfo]::CurrentCulture = New-Object System.Globalization.CultureInfo("pt-BR")
$dataCultural = $dataAtual.ToString("D")
Write-Output $dataCultural
```

Saída de exemplo:

```
03/04/2023 14:55:23
03/04/2023
segunda-feira, 3 de abril de 2023
```

## Detalhamento:
O ato de converter datas em strings não é novo. Em sistemas mais antigos, isso era essencial para documentar e comunicar informações relacionadas ao tempo. No PowerShell, o formato de conversão pode ser flexível e localizado, com o suporte a diferentes culturas.

Existem diferentes formas de especificar o formato:

1. Formatos predefinidos (`"d"`, `"D"`, `"f"`, etc.)
2. Formatos personalizados (usando símbolos como `"dd"`, `"MM"`, `"yyyy"`)

É válido mencionar que o .NET oferece um suporte robusto para trabalhar com datas e strings, o qual o PowerShell aproveita. A internacionalização é uma consideração importante aqui, e o PowerShell permite configurar a cultura para respeitar os formatos de datas locais.

Alternativas ao `ToString()` incluem o uso de `-Format` com cmdlets, ou ainda, o uso de métodos de formatação de strings como o `-f` operator.

```PowerShell
# Uso de -Format com cmdlets
Write-Output (Get-Date -Format "yyyy-MM-dd")

# Uso do operador -f
$stringFormat = "{0:dd MMM yyyy}" -f (Get-Date)
Write-Output $stringFormat
```

## Ver Também:
- [Culturas e Formatos de Data em .NET](https://docs.microsoft.com/pt-br/dotnet/standard/base-types/standard-date-and-time-format-strings)

- [Formatos Customizados em .NET](https://docs.microsoft.com/pt-br/dotnet/standard/base-types/custom-date-and-time-format-strings)
