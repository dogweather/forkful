---
title:                "Obtendo a data atual"
html_title:           "C: Obtendo a data atual"
simple_title:         "Obtendo a data atual"
programming_language: "PowerShell"
category:             "PowerShell"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/powershell/getting-the-current-date.md"
---

{{< edit_this_page >}}

# PowerShell: Como obter a data atual

## O que é e por quê?

Obter a data atual no PowerShell envolve usar comandos específicos para retornar o momento presente exato. Programadores fazem isso para rastrear eventos, registrar horários ou manipular datas e horas em suas soluções de software.

## Como fazer:

Use o comando `Get-Date` para obter a data e a hora atuais. Veja a seguir:

```PowerShell
# Obter data e hora atual
Get-Date

# Saída
Quarta-feira, 9 de fevereiro de 2022 14:32:04
```

Você também pode formatar a data de acordo com sua necessidade com `Get-Date -Format`:

```PowerShell
# Formatar a data
Get-Date -Format "dd-MM-yyyy"

# Saída
09-02-2022
```

## Análise mais profunda:

O `Get-Date` é um cmdlet introduzido no PowerShell v1.0 e continua sendo a principal maneira de obter ou manipular a data e a hora no PowerShell.

Vale a pena mencionar que existem algumas alternativas, que incluem o acesso direto ao .NET Framework usando `[System.DateTime]::Now`:

```PowerShell
# Alternativa usando o .NET Framework
[System.DateTime]::Now

# Saída
09/02/2022 14:32:04
```

Finalmente, a implementação do `Get-Date` é feita por meio de chamadas ao .NET Framework, que tem amplo suporte para operações de data e hora.

## Veja também:

Para mais detalhes e funcionalidades avançadas de trabalho com datas e horas no PowerShell, aqui estão alguns links úteis:

- Documentação oficial do `Get-Date`: https://docs.microsoft.com/pt-br/powershell/module/microsoft.powershell.utility/get-date?view=powershell-7.1
- Trabalhando com datas e horas no PowerShell: https://devblogs.microsoft.com/scripting/working-with-dates-and-times-in-powershell/
- PowerShell Basics: `Get-Date`: https://www.red-gate.com/simple-talk/sysadmin/powershell/powershell-basics-working-with-dates-and-times/