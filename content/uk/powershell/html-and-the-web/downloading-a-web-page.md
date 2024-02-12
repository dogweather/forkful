---
title:                "Завантаження веб-сторінки"
date:                  2024-01-20T17:45:16.745942-07:00
model:                 gpt-4-1106-preview
simple_title:         "Завантаження веб-сторінки"

tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/powershell/downloading-a-web-page.md"
---

{{< edit_this_page >}}

## Що це таке і навіщо?

Завантаження веб-сторінки – це процес, під час якого ви отримуєте вміст сторінки через Інтернет. Програмісти роблять це для автоматизації збору даних, моніторингу контенту або тестування веб-сервісів.

## Як це зробити:

В PowerShell є зручний спосіб завантаження веб-сторінок через Інтернет. Ось як це зробити:

```PowerShell
# Use Invoke-WebRequest to download webpage content
$response = Invoke-WebRequest -Uri "http://example.com"

# Display the raw content
$response.Content

# If you want just the text without HTML tags
$response.ParsedHtml.body.innerText
```

Ви отримаєте щось на кшталт:

```
<!DOCTYPE html>
<html>
...
</html>
```

Або для тексту:

```
Sample text from the web page, without the HTML markup.
```

## Поглиблений Розгляд

Давайте докладніше розглянемо завантаження веб-сторінок. 

1. **Історичний контекст**: Колись для цього використовувались засоби на зразок `curl` або `wget` в Unix-подібних системах. PowerShell вніс легкість використання з Invoke-WebRequest та Invoke-RestMethod.

2. **Альтернативи**: Обрати спосіб завантаження сторінки можна в залежності від потреб. `curl` доступний у PowerShell через псевдонім `curl`, який насправді веде на `Invoke-WebRequest`.

3. **Деталі реалізації**: `Invoke-WebRequest` вважається командою з великою кількістю можливостей, яка допомагає не лише завантажувати контент, а й працювати з веб-формами та сесіями. Для простих запитів можна використати `Invoke-RestMethod`, яка більше підходить для роботи з API.

## Дивіться також

- Офіційна документація Microsoft по `Invoke-WebRequest` та `Invoke-RestMethod`: [Web Cmdlets](https://docs.microsoft.com/powershell/module/Microsoft.PowerShell.Utility/Invoke-WebRequest)
- Ознайомлення з `curl` для Unix/Linux користувачів: [curl](https://curl.se/)
- Розуміння HTTP запитів і веб-взаємодій з добіркою посібників: [HTTP | MDN](https://developer.mozilla.org/en-US/docs/Web/HTTP)
