---
title:                "Запис в стандартний потік помилок"
date:                  2024-01-19
html_title:           "Arduino: Запис в стандартний потік помилок"
simple_title:         "Запис в стандартний потік помилок"
programming_language: "PowerShell"
category:             "PowerShell"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/powershell/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## What & Why?
Стандартна помилка (stderr) – це окремий потік, де програми записують повідомлення про помилки. Використовують її, аби виділяти помилки зі стандартного виводу (stdout) для зручнішої обробки та діагностики.

## How to:
Вивести повідомлення в stderr:

```PowerShell
Write-Error "Це повідомлення про помилку"
```

Перехоплення stderr у файл:

```PowerShell
Some-Command 2> errors.txt
```

Вивід об’єкта у stderr:

```PowerShell
$ErrorObj = [System.Management.Automation.ErrorRecord]::new(
    [Exception]::new("Помилка"),
    "ErrorId",
    [System.Management.Automation.ErrorCategory]::NotSpecified,
    $null
)
$PSCmdlet.WriteError($ErrorObj)
```

## Deep Dive
`Write-Error` в PowerShell існує з самого початку. Це один із способів взаємодії з stderr. Існують інші, наприклад, `[Console]::Error.WriteLine()`. Запис у stderr не зупинить виконання скрипта, на відміну від `throw`.

## See Also
1. [about_Redirection](https://docs.microsoft.com/powershell/module/microsoft.powershell.core/about/about_Redirection)
2. [about_Throw](https://docs.microsoft.com/powershell/module/microsoft.powershell.core/about/about_Throw)
3. [Console.Error](https://docs.microsoft.com/dotnet/api/system.console.error)
