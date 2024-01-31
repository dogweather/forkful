---
title:                "Написання тестів"
date:                  2024-01-19
html_title:           "Arduino: Написання тестів"
simple_title:         "Написання тестів"

tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/powershell/writing-tests.md"
---

{{< edit_this_page >}}

## Що та чому?
Тестування коду — це процес перевірки правильності роботи скриптів. Програмісти пишуть тести, щоб запевнитися у надійності програм та запобігти помилкам у майбутньому.

## Як робити:
Для автоматизованого тестування коду у PowerShell використовують модуль Pester. Ось базовий приклад:

```PowerShell
# Встановлення модуля Pester
Install-Module -Name Pester -Force -SkipPublisherCheck

# Створення тесту
Describe "Тестування функції 'Add-Numbers'" {
    function Add-Numbers {
        param([int]$x, [int]$y)
        return $x + $y
    }

    It "додає число 2 до 2" {
        Add-Numbers 2 2 | Should -Be 4
    }

    It "додає від'ємні числа" {
        Add-Numbers -1 -1 | Should -Be -2
    }
}

# Запуск тесту
Invoke-Pester
```

При запуску цього коду в консолі видасться звіт про результати тестів.

## Поглиблений розгляд:
Pester — це модуль для PowerShell з 2009 року. Це найпопулярніша у спільноті фреймворк для тестування. Є альтернативи типу PSUnit, але Pester вважається "золотим стандартом". Пестер дозволяє писати unit-tests, integration tests та mock objects.

## Ще ресурси:
- [Офіційна документація Pester](https://pester.dev/docs/quick-start)
- [Книга "The Pester Book" за авторством Дона Джонса](https://leanpub.com/pesterbook)
- [PowerShell.org форум для обміну досвідом](https://powershell.org/forums/)
