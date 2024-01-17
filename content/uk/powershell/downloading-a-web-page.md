---
title:                "Завантаження веб-сторінки"
html_title:           "PowerShell: Завантаження веб-сторінки"
simple_title:         "Завантаження веб-сторінки"
programming_language: "PowerShell"
category:             "PowerShell"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/powershell/downloading-a-web-page.md"
---

{{< edit_this_page >}}

## Що і чому?

Завантаження веб сторінки - це процес отримання вмісту з інтернет-ресурсу і збереження його на вашому комп'ютері. При цьому, програмісти до цього прибігають, щоб отримати необхідну інформацію для подальшої обробки або аналізу.

## Як:

```PowerShell
Invoke-WebRequest http://example.com | Out-File website.html
```
В цьому прикладі ми використали команду `Invoke-WebRequest`, щоб отримати вміст з веб-сторінки і вивести його в файл `website.html`.

## Глибоке погруження:

1. Історичний контекст: в минулому, для завантаження веб-сторінок програмістам доводилося використовувати складні мови програмування, такі як C і Java. Однак у сучасних мов програмування, таких як PowerShell, цей процес значно спрощено.

2. Альтернативи: поміж іншими способами завантаження веб-сторінок є використання REST API або HTTP запитів безпосередньо з мови програмування.

3. Деталі реалізації: для завантаження веб-сторінок в PowerShell використовується модуль `Microsoft.PowerShell.Utility`, який дозволяє використовувати команди `Invoke-WebRequest` та `Invoke-RestMethod`.

## Дивись також:

Відповідальним при писанні скрипту для завантаження веб-сторінки є стеження за змістом файлу, щоб упевнитися, що отримана інформація є цілісною. Приклади з цими командами можна знайти на офіційному сайті [PowerShell](https://docs.microsoft.com/en-us/powershell/module/microsoft.powershell.utility/invoke-webrequest) та в [блозі](https://adamtheautomator.com/invokewebrequest/) Адама Берта про `Invoke-WebRequest` і подібні команди.