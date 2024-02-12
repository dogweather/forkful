---
title:                "Использование интерактивной оболочки (REPL)"
aliases: - /ru/powershell/using-an-interactive-shell-repl.md
date:                  2024-01-29T00:03:57.528482-07:00
model:                 gpt-4-0125-preview
simple_title:         "Использование интерактивной оболочки (REPL)"

tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ru/powershell/using-an-interactive-shell-repl.md"
changelog:
  - 2024-01-29, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Что и зачем?
Интерактивная оболочка или цикл "чтение-выполнение-вывод" (REPL) позволяет вводить команды PowerShell и получать немедленную обратную связь. Программисты используют её для быстрого тестирования фрагментов кода, отладки или изучения новых команд без написания полного скрипта.

## Как использовать:
Запустите PowerShell, и вы окажетесь в REPL. Попробуйте Cmdlet `Get-Date`:

```PowerShell
PS > Get-Date
```

Вы должны увидеть вывод текущей даты и времени:

```PowerShell
Среда, 31 марта 2023 г. 12:34:56
```

Теперь соедините команды. Давайте отсортируем процессы по использованию памяти:

```PowerShell
PS > Get-Process | Sort-Object WS -Descending | Select-Object -First 5
```

Это выведет пять процессов с наибольшим размером рабочего набора (использование памяти).

## Глубокое погружение
REPL в PowerShell имеет корни в Unix-оболочке и других динамических языках, таких как оболочка Python. Это среда интерактивного выполнения команд для одного пользователя. В отличие от компилируемых языков, где вы пишете целые приложения, а затем компилируете их, среда REPL позволяет писать и выполнять код построчно. PowerShell также поддерживает выполнение скриптов для более крупных задач.

Альтернативы для Windows включают командную строку или другие специфичные для языка REPL, такие как IPython. В мире Unix/Linux подобную функцию выполняют оболочки, такие как bash или zsh.

Реализация REPL в PowerShell использует хост-приложение для запуска оболочки. Хотя наиболее распространенным является PowerShell.exe в Windows, другие, как Объединённая среда скриптов (ISE) или интегрированный терминал Visual Studio Code, также могут служить хостом.

## См. также
- [О PowerShell](https://docs.microsoft.com/en-us/powershell/scripting/overview)
- [StackOverflow: PowerShell](https://stackoverflow.com/questions/tagged/powershell)
