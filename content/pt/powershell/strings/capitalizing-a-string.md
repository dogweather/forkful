---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:05:58.976186-07:00
description: "Capitalizar uma string no PowerShell envolve transformar o primeiro\
  \ caractere de uma string dada em mai\xFAsculo, deixando o restante da string inalterado.\u2026"
lastmod: '2024-03-13T22:44:46.778600-06:00'
model: gpt-4-0125-preview
summary: "Capitalizar uma string no PowerShell envolve transformar o primeiro caractere\
  \ de uma string dada em mai\xFAsculo, deixando o restante da string inalterado."
title: Capitalizando uma string
weight: 2
---

## O que & Por quê?
Capitalizar uma string no PowerShell envolve transformar o primeiro caractere de uma string dada em maiúsculo, deixando o restante da string inalterado. Programadores frequentemente realizam essa tarefa para fins de formatação, como preparar texto para exibição em interfaces de usuário ou seguir regras gramaticais em documentos gerados.

## Como fazer:
O PowerShell, sendo uma ferramenta versátil, permite que você capitalize uma string usando métodos diretos sem a necessidade de bibliotecas de terceiros. Veja como você pode fazer isso:

```powershell
# Usando o método .Net embutido 'ToTitleCase' de CultureInfo
$text = "hello world"
$culture = [System.Globalization.CultureInfo]::InvariantCulture
$capitalizedText = $culture.TextInfo.ToTitleCase($text.ToLower())
Write-Output $capitalizedText
```
Saída:
```
Hello world
```

Nota: Este método capitaliza a primeira letra de cada palavra. Se você deseja estritamente capitalizar apenas a primeira letra da string e deixar o restante como está, você poderia fazer algo assim:

```powershell
# Capitalizando apenas o primeiro caractere de uma string
$text = "hello world"
$capitalizedText = $text.Substring(0,1).ToUpper() + $text.Substring(1)
Write-Output $capitalizedText
```
Saída:
```
Hello world
```

O PowerShell não inclui diretamente uma função simples para capitalizar apenas a primeira letra de uma string, mas combinando os métodos básicos de manipulação de strings como `Substring(0,1).ToUpper()` e concatenação, podemos facilmente alcançar o resultado desejado.
