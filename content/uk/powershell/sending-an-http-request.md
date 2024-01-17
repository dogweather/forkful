---
title:                "Надсилання http запиту"
html_title:           "PowerShell: Надсилання http запиту"
simple_title:         "Надсилання http запиту"
programming_language: "PowerShell"
category:             "PowerShell"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/powershell/sending-an-http-request.md"
---

{{< edit_this_page >}}

Що і чому?
Відправка HTTP-запиту є процесом взаємодії з веб-сервером, коли вам потрібно отримати інформацію з Інтернету, наприклад, сторінку веб-сайту чи дані з API. Програмісти роблять це для того, щоб отримати доступ до потрібної інформації і використовувати її в своїх програмах.

Як це зробити:
```PowerShell
# Надсилання GET-запиту та виведення статусного коду
Invoke-RestMethod -Uri "https://example.com/api" -Method GET

# Надсилання POST-запиту з параметрами та виведення відповіді в JSON-форматі
$body = @{
    name = "John"
    age = 25
}
Invoke-RestMethod -Uri "https://example.com/api" -Method POST -Body $body | ConvertTo-Json
```

Вглиб?
Історичний контекст: перед появою PowerShell, програмісти використовували інструменти, такі як cURL або wget, для взаємодії з веб-серверами. Однак, з випуском PowerShell 3.0, був доданий модуль `Microsoft.PowerShell.Utility`, який включає команду `Invoke-WebRequest` для відправки HTTP-запитів.

Альтернативи: окрім `Invoke-RestMethod`, ви також можете використовувати `Invoke-WebRequest` для взаємодії з веб-серверами. Ця команда дає вам більшу гнучкість та можливість налаштовувати параметри запиту.

Деталі реалізації: при використанні `Invoke-RestMethod`, PowerShell автоматично здійснює перетворення відповіді в об'єкт PowerShell, що полегшує обробку даних. Крім того, ви можете використовувати параметр `-Headers` для передачі додаткової інформації, такої як заголовки HTTP-запиту.

Як бачите, надсилання HTTP-запитів у PowerShell досить просто, але зручно для взаємодії з веб-серверами. Будьте впевнені у своїх даних та бережіться SQL-ін'єкцій, коли отримуєте вхідні дані для відправки в запиті.

Дивіться також:
- [Документація Microsoft для Invoke-RestMethod](https://docs.microsoft.com/en-us/powershell/module/microsoft.powershell.utility/invoke-restmethod)
- [Документація Microsoft для Invoke-WebRequest](https://docs.microsoft.com/en-us/powershell/module/microsoft.powershell.utility/invoke-webrequest)