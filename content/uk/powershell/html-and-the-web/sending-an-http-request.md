---
title:                "Надсилання HTTP-запиту"
aliases:
- /uk/powershell/sending-an-http-request/
date:                  2024-01-20T18:00:31.061709-07:00
model:                 gpt-4-1106-preview
simple_title:         "Надсилання HTTP-запиту"

tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/powershell/sending-an-http-request.md"
---

{{< edit_this_page >}}

## Що і Чому?
Відправка HTTP-запиту дозволяє вашому скрипту PowerShell "спілкуватися" із веб-серверами: отримувати дані, відправляти їх, аутентифікуватися.Програмісти роблять це, щоб автоматизувати взаємодію з API, збирати дані з Інтернету або перевіряти доступність веб-ресурсів.

## Як Робити:
```PowerShell
# Отримуємо вміст веб-сторінки
$response = Invoke-RestMethod -Uri 'http://example.com'
Write-Host "Відповідь: $response"

# Відправляємо POST-запит з даними форми
$body = @{username='юзер'; password='пароль'}
$response = Invoke-RestMethod -Uri 'http://example.com/login' -Method 'POST' -Body $body
Write-Host "Дані користувача: $response"

# Використовуємо заголовки для передачі API ключа
$headers = @{ 'API-Key'='секретний_ключ' }
$response = Invoke-RestMethod -Uri 'http://api.example.com/data' -Headers $headers
Write-Host "Дані з API: $response"
```
Готово. Просто і чітко.

## Підводимо Підсумки:
Відправлення HTTP-запитів - основна частина веб-автоматизації. Раніше для цього використовували командлети як `Invoke-WebRequest`. `Invoke-RestMethod` з'явився у PowerShell 3.0 і оптимізований для роботи з API, що повертають JSON чи XML. Є й альтернативи, такі як `curl` чи `wget`, але вони можуть вимагати додаткової установки чи налаштувань.

## Дивіться Також:
- [Invoke-RestMethod документація](https://docs.microsoft.com/powershell/module/microsoft.powershell.utility/invoke-restmethod)
- [Про HTTP методи](https://developer.mozilla.org/docs/Web/HTTP/Methods)
- [REST API концепції](https://www.redhat.com/en/topics/api/what-is-a-rest-api)
