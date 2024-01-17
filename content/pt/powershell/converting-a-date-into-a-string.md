---
title:                "Convertendo uma data em uma string"
html_title:           "PowerShell: Convertendo uma data em uma string"
simple_title:         "Convertendo uma data em uma string"
programming_language: "PowerShell"
category:             "PowerShell"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/powershell/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## O que é isso e por quê?

Converter uma data em uma string é um processo no qual um programador transforma uma data (formato numérico) em uma representação de texto. Os programadores muitas vezes fazem isso para tornar a data mais fácil de ler e entender em um formato legível para humanos.

## Como fazer:

```PowerShell
# Exemplo usando o cmdlet Get-Date
Get-Date -Format "dd/MM/yyyy"

# Saída: 26/10/2021

# Exemplo usando um objeto DateTime
$data = Get-Date
$data.ToString("dddd, d MMMM yyyy")

# Saída: terça-feira, 26 outubro 2021
```

## Profundidade:

Conversão de data para string é um conceito importante na programação, especialmente quando se trabalha com datas e horários. É uma prática comum em muitas linguagens de programação, incluindo o PowerShell. Existem várias maneiras de converter uma data em uma string, como demonstrado acima, mas é importante escolher o método certo para garantir a precisão e a compreensão correta da data.

## Veja também:

- [Documentação do cmdlet Get-Date](https://docs.microsoft.com/pt-br/powershell/module/microsoft.powershell.utility/get-date?view=powershell-7.1)
- [Métodos de formatação de data e hora em C#](https://docs.microsoft.com/pt-br/dotnet/standard/base-types/custom-date-and-time-format-strings)