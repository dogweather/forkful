---
title:                "Calculando uma data no futuro ou passado"
html_title:           "PowerShell: Calculando uma data no futuro ou passado"
simple_title:         "Calculando uma data no futuro ou passado"
programming_language: "PowerShell"
category:             "PowerShell"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/powershell/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## O Que é e Porquê? 
Calcular uma data no futuro ou passado é uma operação comum que nos permite manipular datas para encontrar um ponto específico no tempo. Programadores fazem isso para agendar eventos, calcular prazos, entre outras coisas.

## Como Fazer:
Você pode usar o cmdlet `Add-Days` para calcular uma data futura ou passada. Aqui está um exemplo:

```PowerShell
# Data atual
$dataAtual = Get-Date
Escrever-Host "Data atual: $dataAtual"

# Calcular uma data no futuro
$dataFutura = $dataAtual.AddDays(10)
Escrever-Host "Data futura: $dataFutura"

# Calcular uma data no passado
$dataPassada = $dataAtual.AddDays(-10)
Escrever-Host "Data passada: $dataPassada"
```

A saída será:

```PowerShell
Data atual: 02/02/2022 12:10:00
Data futura: 12/02/2022 12:10:00
Data passada: 23/01/2022 12:10:00
```

## Aprofundamento:
Calcular datas no futuro ou passado é uma técnica que tem sido usada em programação desde os primórdios do COBOL. Hoje em dia, existem outras formas de fazer isso em PowerShell, como usando o cmdlet `Set-Date`.

No entanto, `Add-Days` é recomendado por ser mais flexível e fácil de usar. Além disso, `Add-Days` faz parte do objeto DateTime do .NET, portanto, ele será compatível com qualquer versão do Windows que suporte o .NET Framework ou .NET Core.

## Veja Também:
Aqui estão alguns links para fontes relacionadas para obter mais informações:


   
3. [Working with PowerShell Date Command (Get-Date)](https://adamtheautomator.com/powershell-get-date/)