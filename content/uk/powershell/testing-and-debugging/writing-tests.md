---
title:                "Письмо тестів"
date:                  2024-02-03T19:31:54.174033-07:00
model:                 gpt-4-0125-preview
simple_title:         "Письмо тестів"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/powershell/writing-tests.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Що і чому?

Написання тестів у PowerShell передбачає створення скриптів, які автоматично перевіряють функціональність вашого коду PowerShell, забезпечуючи його очікувану поведінку. Програмісти роблять це для того, щоб якомога раніше виявити помилки, спростити обслуговування коду та забезпечити, щоб модифікації коду не порушували вже існуючу функціональність.

## Як це зробити:

PowerShell не має вбудованого фреймворку для тестування, але Pester, популярний сторонній модуль, широко використовується для написання та виконання тестів. Ось як почати використовувати Pester для тестування ваших функцій PowerShell.

Спочатку встановіть Pester, якщо ви ще цього не зробили:

```powershell
Install-Module -Name Pester -Scope CurrentUser -Force
```

Далі, припустимо, що у вас є проста функція PowerShell, яку ви хочете протестувати, збережена як `MyFunction.ps1`:

```powershell
function Get-MultipliedNumber {
    param (
        [int]$Number,
        [int]$Multiplier = 2
    )

    return $Number * $Multiplier
}
```

Для тестування цієї функції за допомогою Pester, створіть тестовий скрипт під назвою `MyFunction.Tests.ps1`. У цьому скрипті використовуйте блоки `Describe` та `It` в Pester, щоб визначити тестові випадки:

```powershell
# Імпортуйте функцію для тестування
. .\MyFunction.ps1

Describe "Тести Get-MultipliedNumber" {
    It "Множить число на 2, коли множник не заданий" {
        $result = Get-MultipliedNumber -Number 3
        $result | Should -Be 6
    }

    It "Коректно множить число на заданий множник" {
        $result = Get-MultipliedNumber -Number 3 -Multiplier 3
        $result | Should -Be 9
    }
}
```

Щоб запустити тести, відкрийте PowerShell, перейдіть до каталогу з вашим тестовим скриптом та використайте команду `Invoke-Pester`:

```powershell
Invoke-Pester .\MyFunction.Tests.ps1
```

Результати виглядатимуть так, вказуючи, чи пройшли ваші тести або вони зазнали невдачі:

```
Початок відкриття в 1 файлах.
Відкриття завершено за 152ms.
[+] C:\шлях\до\MyFunction.Tests.ps1 204ms (182ms|16ms)
Тести завершено за 204ms
Тести Пройдено: 2, Провалено: 0, Пропущено: 0 Не Виконано: 0
```

Ці результати показують, що обидва тести пройшли, даючи вам впевненість в тому, що ваша функція `Get-MultipliedNumber` поводиться як очікується у сценаріях, які ви тестували.
