---
title:                "Використання інтерактивної оболонки (REPL)"
aliases:
- uk/powershell/using-an-interactive-shell-repl.md
date:                  2024-01-26T04:17:37.824439-07:00
model:                 gpt-4-0125-preview
simple_title:         "Використання інтерактивної оболонки (REPL)"

tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/powershell/using-an-interactive-shell-repl.md"
---

{{< edit_this_page >}}

## Що і чому?
Інтерактивний оболонка або цикл "читати-виконати-вивести" (REPL) дозволяє вводити команди PowerShell і отримувати негайну відповідь. Програмісти використовують його для швидкого тестування фрагментів коду, відлагодження або вивчення нових команд без написання повного скрипта.

## Як це зробити:
Запустіть PowerShell, і ви потрапите до REPL. Спробуйте командлет `Get-Date`:

```PowerShell
PS > Get-Date
```

Ви повинні побачити вивід поточної дати та часу:

```PowerShell
Середа, 31 березня 2023 12:34:56
```

Тепер, поєднайте команди. Давайте сортуватимемо процеси за використанням пам'яті:

```PowerShell
PS > Get-Process | Sort-Object WS -Descending | Select-Object -First 5
```

Це виведе п'ять процесів з найбільшим розміром використаної пам'яті (використання пам'яті).

## Поглиблений аналіз
REPL PowerShell має свої корені в оболонці Unix та інших динамічних мовних оболонках, таких як Python. Це однокористувацьке, інтерактивне середовище виконання команд. На відміну від компільованої мови, де ви пишете цілі додатки, а потім компілюєте, середовище REPL дозволяє писати і виконувати код по одному рядку за раз. PowerShell також підтримує виконання скриптів для більших завдань.

Альтернативи для Windows включають Командний рядок або інші специфічні для мови REPL, як IPython. У світі Unix/Linux оболонки на кшталт bash або zsh виконують схожу функцію.

Реалізація PowerShell використовує хост-додаток для запуску оболонки. Хоча PowerShell.exe у Windows є найпоширенішим, інші, як-от Інтегроване середовище скриптів (ISE) або інтегрований термінал Visual Studio Code, також можуть слугувати хостом.

## Дивіться також
- [Про PowerShell](https://docs.microsoft.com/en-us/powershell/scripting/overview)
- [StackOverflow: PowerShell](https://stackoverflow.com/questions/tagged/powershell)
