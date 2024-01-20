---
title:                "Надсилання http-запиту"
html_title:           "Arduino: Надсилання http-запиту"
simple_title:         "Надсилання http-запиту"
programming_language: "PowerShell"
category:             "PowerShell"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/powershell/sending-an-http-request.md"
---

{{< edit_this_page >}}

## Що & навіщо?

Відправлення HTTP-запиту це запит до веб-сервера для отримання або надсилання даних. Програмісти роблять це, щоб взаємодіяти з API, веб-сайтами та іншими веб-сервісами.

## Як це робити:

Отже, як ви можете відправити HTTP-запит за допомогою PowerShell? Дуже просто. Використовуйте командлет `Invoke-WebRequest`.

```PowerShell
# Все, що вам потрібно, це URI
$uri = 'http://example.com'

# Надсилаємо запит GET
$response = Invoke-WebRequest -Uri $uri

# Виводимо відповідь
$response.StatusCode
$response.StatusDescription
```

Коли ви виконуєте цей код, ви отримаєте вивід:

```PowerShell
200
OK
```

## Більш глибокий огляд:

PowerShell почав використовувати HTTP-запити з її появи в 2006 році, щоб полегшити взаємодію з веб-сервісами.

Головною альтернативою є командлет `Invoke-RestMethod`, який аналізує JSON-відповіді автоматично.

Під капотом, `Invoke-WebRequest` використовує .NET класи `System.Net.WebRequest` та `System.Net.WebResponse` для надсилання HTTP-запитів.

## Дізнайтеся більше:

1. [Повний огляд командлета `Invoke-WebRequest`](https://docs.microsoft.com/en-us/powershell/module/microsoft.powershell.utility/invoke-webrequest?view=powershell-7.1)
2. [Альтернативне використання `Invoke-RestMethod`](https://docs.microsoft.com/en-us/powershell/module/microsoft.powershell.utility/invoke-restmethod?view=powershell-7.1)