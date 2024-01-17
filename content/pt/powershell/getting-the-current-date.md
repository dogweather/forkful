---
title:                "Obtendo a data atual"
html_title:           "PowerShell: Obtendo a data atual"
simple_title:         "Obtendo a data atual"
programming_language: "PowerShell"
category:             "PowerShell"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/powershell/getting-the-current-date.md"
---

{{< edit_this_page >}}

## O que e por que?

Obter a data atual é um processo simples e útil para programadores. É a forma de obter a data e hora exatas do momento em que o código está sendo executado. Isso é útil para registrar eventos, validar a idade de um usuário e muito mais.

## Como fazer:

Para obter a data atual no PowerShell, podemos usar o cmdlet `Get-Date`. Veja um exemplo abaixo:

```PowerShell
Get-Date
```
Este comando irá retornar a data atual no formato padrão do seu sistema, como por exemplo: `06/08/2021 14:30:00`.

Se você quiser formatar a saída de acordo com suas preferências, pode usar a opção `-Format` seguida de um formato de data e hora. Por exemplo:

```PowerShell
Get-Date -Format "dd/MM/yyyy"
```

Isso retornará apenas a data atual no formato `06/08/2021`.

## Profundando:

Embora seja uma tarefa simples, obter a data atual pode ser útil em muitos cenários, como monitorar a criação de arquivos, programação de tarefas e atualizações em tempo real.

Além disso, existem outras maneiras de obter a data atual no PowerShell, como usando as funções `Get-Item` e `Get-ChildItem`, que podem ser mais adequadas dependendo do seu caso de uso.

Para implementar a obtenção da data atual, o PowerShell faz uso do .NET Framework para acessar o relógio do sistema operacional e retornar a data e hora atuais.

## Veja também:

- https://docs.microsoft.com/en-us/powershell/module/microsoft.powershell.utility/get-date?view=powershell-7.1
- https://docs.microsoft.com/en-us/powershell/scripting/learn/deep-dives/everything-about-dates?view=powershell-7.1