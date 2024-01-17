---
title:                "Calculando uma data no futuro ou no passado."
html_title:           "PowerShell: Calculando uma data no futuro ou no passado."
simple_title:         "Calculando uma data no futuro ou no passado."
programming_language: "PowerShell"
category:             "PowerShell"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/powershell/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## O que & Porquê?

Calcular uma data no futuro ou passado é uma tarefa comum em muitos programas. É a maneira de adicionar ou subtrair um determinado número de dias, semanas, meses ou anos a uma data específica. Isso permite que os programadores criem lógica de data dinâmica em seus scripts e automações.

## Como fazer:

```PowerShell
# Calculando 30 dias no futuro a partir de hoje
(Get-Date).AddDays(30)

# Calculando 2 anos e 5 meses no passado a partir de uma data específica
(Get-Date "01/01/2020").AddYears(-2).AddMonths(-5)

# Calculando 2 semanas no futuro a partir de uma data digitada pelo usuário
$inputDate = Read-Host "Insira uma data (formato: MM/DD/YYYY)"
(Get-Date $inputDate).AddDays(14)

# Saída: Segunda-feira, 13 de julho de 2020 00:00:00
```

## Mergulho profundo:

Calculando datas no futuro ou passado é uma função importante em muitos programas. Historicamente, foi uma tarefa tediosa para os programadores, pois envolvia muitas conversões de data e cálculos complexos. No entanto, com o PowerShell, o processo é muito mais simples e menos propenso a erros. Existem também alternativas, como o uso de módulos de terceiros ou a criação de funções personalizadas para realizar o cálculo de datas.

## Veja também:

- Documentação oficial do PowerShell: https://docs.microsoft.com/pt-br/powershell/scripting/overview?view=powershell-7
- Blog do Microsoft sobre manipulação de datas no PowerShell: https://devblogs.microsoft.com/scripting/use-powershell-to-work-with-any-date-format/
- Vídeo tutorial sobre como calcular datas no PowerShell: https://www.youtube.com/watch?v=CbfVrim6S1c