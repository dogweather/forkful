---
date: 2024-01-20 18:00:31.061709-07:00
description: "\u042F\u043A \u0420\u043E\u0431\u0438\u0442\u0438: \u0412\u0456\u0434\
  \u043F\u0440\u0430\u0432\u043B\u0435\u043D\u043D\u044F HTTP-\u0437\u0430\u043F\u0438\
  \u0442\u0456\u0432 - \u043E\u0441\u043D\u043E\u0432\u043D\u0430 \u0447\u0430\u0441\
  \u0442\u0438\u043D\u0430 \u0432\u0435\u0431-\u0430\u0432\u0442\u043E\u043C\u0430\
  \u0442\u0438\u0437\u0430\u0446\u0456\u0457. \u0420\u0430\u043D\u0456\u0448\u0435\
  \ \u0434\u043B\u044F \u0446\u044C\u043E\u0433\u043E \u0432\u0438\u043A\u043E\u0440\
  \u0438\u0441\u0442\u043E\u0432\u0443\u0432\u0430\u043B\u0438 \u043A\u043E\u043C\u0430\
  \u043D\u0434\u043B\u0435\u0442\u0438 \u044F\u043A `Invoke-WebRequest`. `Invoke-\u2026"
lastmod: '2024-04-05T22:51:02.660019-06:00'
model: gpt-4-1106-preview
summary: "\u0412\u0456\u0434\u043F\u0440\u0430\u0432\u043B\u0435\u043D\u043D\u044F\
  \ HTTP-\u0437\u0430\u043F\u0438\u0442\u0456\u0432 - \u043E\u0441\u043D\u043E\u0432\
  \u043D\u0430 \u0447\u0430\u0441\u0442\u0438\u043D\u0430 \u0432\u0435\u0431-\u0430\
  \u0432\u0442\u043E\u043C\u0430\u0442\u0438\u0437\u0430\u0446\u0456\u0457."
title: "\u041D\u0430\u0434\u0441\u0438\u043B\u0430\u043D\u043D\u044F HTTP-\u0437\u0430\
  \u043F\u0438\u0442\u0443"
weight: 44
---

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
