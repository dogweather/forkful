---
title:                "Завантаження веб-сторінки"
html_title:           "Gleam: Завантаження веб-сторінки"
simple_title:         "Завантаження веб-сторінки"
programming_language: "PowerShell"
category:             "PowerShell"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/powershell/downloading-a-web-page.md"
---

{{< edit_this_page >}}

## Що та чому?
Завантаження веб-сторінки полягає в отриманні її даних через Інтернет для подальшого використання або аналізу. Програмісти роблять це, щоб автоматизувати роботу з даними, що знаходяться онлайн.

## Як це зробити:
З PowerPoint вам потрібно лише кілька команд, щоб завантажити веб-сторінку. Ось приклад:

```PowerShell 
# Встановимо URL веб-сторінки
$url = 'http://example.com'

# Використаємо Invoke-WebRequest, щоб отримати дані сторінки
$request = Invoke-WebRequest -Uri $url

# Друкуємо вміст сторінки
$request.Content
```

Коли ви запустите цей код, ви побачите вміст веб-сторінки, який ви завантажили.

## Поглиблений огляд
Розуміння того, як завантажувати веб-сторінки в PowerShell, можливе з деяким історичним контекстом. PowerShell відповідає за автоматизацію і управління Windows, а ця задача стосується автоматизації.

Є альтернативи, як-то використання інших мов програмування (Python, Java тощо) або використання спеціальних інструментів для завантаження сторінок. 

Invoke-WebRequest - це встроєна команда PowerShell, яка використовує .NET Framework для відправки HTTP-запитів. Вона повертає об’єкт, який можна легко маніпулювати всередині PowerShell.

## Дивитись також
- [Офіційна документація PowerShell по Invoke-WebRequest](https://docs.microsoft.com/uk-ua/powershell/module/Microsoft.PowerShell.Utility/Invoke-WebRequest?view=powershell-7.1)
- [Глибоке вивчення автоматизації PowerShell](https://leanpub.com/learnpowershell)
- [Основи скрапінгу веб-сторінок](https://www.freecodecamp.org/news/scraping-ecommerce-website-with-python-and-beautifulsoup/)