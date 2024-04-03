---
date: 2024-01-20 17:45:16.745942-07:00
description: "\u042F\u043A \u0446\u0435 \u0437\u0440\u043E\u0431\u0438\u0442\u0438\
  : \u0412 PowerShell \u0454 \u0437\u0440\u0443\u0447\u043D\u0438\u0439 \u0441\u043F\
  \u043E\u0441\u0456\u0431 \u0437\u0430\u0432\u0430\u043D\u0442\u0430\u0436\u0435\u043D\
  \u043D\u044F \u0432\u0435\u0431-\u0441\u0442\u043E\u0440\u0456\u043D\u043E\u043A\
  \ \u0447\u0435\u0440\u0435\u0437 \u0406\u043D\u0442\u0435\u0440\u043D\u0435\u0442\
  . \u041E\u0441\u044C \u044F\u043A \u0446\u0435 \u0437\u0440\u043E\u0431\u0438\u0442\
  \u0438."
lastmod: '2024-03-13T22:44:49.648062-06:00'
model: gpt-4-1106-preview
summary: "\u0412 PowerShell \u0454 \u0437\u0440\u0443\u0447\u043D\u0438\u0439 \u0441\
  \u043F\u043E\u0441\u0456\u0431 \u0437\u0430\u0432\u0430\u043D\u0442\u0430\u0436\u0435\
  \u043D\u043D\u044F \u0432\u0435\u0431-\u0441\u0442\u043E\u0440\u0456\u043D\u043E\
  \u043A \u0447\u0435\u0440\u0435\u0437 \u0406\u043D\u0442\u0435\u0440\u043D\u0435\
  \u0442."
title: "\u0417\u0430\u0432\u0430\u043D\u0442\u0430\u0436\u0435\u043D\u043D\u044F \u0432\
  \u0435\u0431-\u0441\u0442\u043E\u0440\u0456\u043D\u043A\u0438"
weight: 42
---

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
