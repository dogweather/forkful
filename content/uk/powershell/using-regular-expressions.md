---
title:                "Використання регулярних виразів"
html_title:           "Bash: Використання регулярних виразів"
simple_title:         "Використання регулярних виразів"
programming_language: "PowerShell"
category:             "PowerShell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/powershell/using-regular-expressions.md"
---

{{< edit_this_page >}}

## Що це таке та навіщо?
Регулярні вирази - це мова пошуку та маніпуляції текстом. Програмісти їх використовують для знаходження, заміни, або перевірки шаблонів у рядках. Це економить час і дозволяє обробляти великі обсяги даних швидко та ефективно.

## Як це робити:
```PowerShell
# Знаходимо всі номери телефонів у форматі (xxx) xxx-xxxx
$text = "Зателефонуй мені за номером (123) 456-7890 або (098) 765-4321."
$pattern = '\(\d{3}\) \d{3}-\d{4}'
[regex]::Matches($text, $pattern) | ForEach-Object { $_.Value }

# Вихідні дані:
# (123) 456-7890
# (098) 765-4321

# Заміна всіх електронних адрес
$emailText = "Моя пошта test@example.com, а альтернативна - contact@example.net."
$emailPattern = '[\w.-]+@[a-zA-Z-]+\.[a-zA-Z]+'
[regex]::Replace($emailText, $emailPattern, 'hidden@example.com')

# Вихідні дані:
# Моя пошта hidden@example.com, а альтернативна - hidden@example.com.
```

## Поглиблений аналіз:
Регулярні вирази прийшли з теоретичної інформатики і були популяризовані у Unix утилітах, таких як sed та grep. Альтернативи регулярним виразам включають роботу з текстовими рядками за допомогою основних методів як IndexOf чи Replace, але це часто не настільки гнучко та потужно. PowerShell використовує .NET клас System.Text.RegularExpressions для регулярних виразів, що забезпечує широкий спектр функціональності та оптимізації.

## Дивіться також:
- [Інтерактивний тренажер для навчання регулярних виразів](https://regexr.com/)
- [Довідник регулярних виразів](https://www.regular-expressions.info/)