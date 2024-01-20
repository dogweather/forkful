---
title:                "Друк відлагоджувального виводу"
html_title:           "Arduino: Друк відлагоджувального виводу"
simple_title:         "Друк відлагоджувального виводу"
programming_language: "PowerShell"
category:             "PowerShell"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/powershell/printing-debug-output.md"
---

{{< edit_this_page >}}

## Що це та навіщо?

Це виведення відлагоджувальної інформації, яка допомагає програмістам слідкувати за виконанням коду та виявляти помилки. Цей процес значно полегшує відладку та забезпечує більше контролю над роботою програми.

## Як це зробити:

Ось простий приклад використання вбудованого параметра `Write-Debug` в PowerShell:

```PowerShell
function Test-Func{
    [cmdletbinding()]
    param()

    Write-Debug "Це повідомлення для відлагодження"
}

$DebugPreference = 'Continue'
Test-Func
```

В результаті ви побачите наступне повідомлення:

```PowerShell
DEBUG: Це повідомлення для відлагодження
```

## Поглиблений огляд:

Друк відлагоджувального виведення має глибокі корені в історії програмування і дозволяє вивести додаткову інформацію з програми без втручання в її основну роботу. За межами PowerShell, це можна реалізувати за допомогою таких методів як `console.log()` в JavaScript або `print()` в Python. 

В PowerShell можна використовувати кілька способів для виведення відлагоджувальної інформації, у тому числі `Write-Debug`, `Write-Verbose`, `Write-Warning`, `Write-Error`, і `Write-Information`. Ці методи корисні, щоб відстежувати хід виконання програми та повідомляти про статуси помилок в процесі.

## Дивіться також:

1. [Write-Debug (docs.microsoft.com)](https://docs.microsoft.com/en-us/powershell/module/microsoft.powershell.utility/write-debug?view=powershell-7.1)