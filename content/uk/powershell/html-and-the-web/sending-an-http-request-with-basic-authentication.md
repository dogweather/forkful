---
date: 2024-01-20 18:02:45.339018-07:00
description: "\u042F\u043A \u0417\u0440\u043E\u0431\u0438\u0442\u0438: ."
lastmod: '2024-03-13T22:44:49.649751-06:00'
model: gpt-4-1106-preview
summary: .
title: "\u041D\u0430\u0434\u0441\u0438\u043B\u0430\u043D\u043D\u044F HTTP-\u0437\u0430\
  \u043F\u0438\u0442\u0443 \u0437 \u0431\u0430\u0437\u043E\u0432\u043E\u044E \u0430\
  \u0432\u0442\u0435\u043D\u0442\u0438\u0444\u0456\u043A\u0430\u0446\u0456\u0454\u044E"
weight: 45
---

## Як Зробити:
```PowerShell
# Define Base64-encoded credentials
$user = 'username'
$pass = 'password'
$pair = "$($user):$($pass)"
$encodedCreds = [System.Convert]::ToBase64String([System.Text.Encoding]::ASCII.GetBytes($pair))

# Create a headers dictionary with Basic Authorization
$headers = New-Object "System.Collections.Generic.Dictionary[[String],[String]]"
$headers.Add("Authorization", "Basic $encodedCreds")

# Define the URL to send the request to
$url = 'https://api.example.com/data'

# Send an HTTP GET request with basic authentication
$response = Invoke-RestMethod -Headers $headers -Method Get -Uri $url

# Output the response
$response
```
Цей код відправить запит і виведе отриману відповідь.

## Поглиблений Розгляд:
Базова автентифікація — давний метод, який передбачає надсилання логіна та пароля у відкритому тексті у кодуванні Base64. Хоча він і простий у використанні, через не зашифровані дані, його сьогодні часто замінюють безпечнішими методами, наприклад OAuth.

PowerShell використовує `Invoke-RestMethod` для відправлення HTTP запитів. Таке командлет має багато параметрів для керування запитами, включаючи заголовки та тіло запиту. У нашому випадку, ми додаємо заголовок `Authorization` із закодованими у Base64 логіном та паролем.

Замість прямого кодування логіна і пароля можна використовувати більш безпечні методи, як от "SecureString" чи шифрування цілих конфігураційних файлів.

Існує більше варіантів HTTP командлетів, які містять `WebClient` або `HttpWebRequest`. Однак `Invoke-RestMethod` є спеціалізованим інструментом для роботи з REST API, що робить його ідеальним для взаємодії з сучасними веб-сервісами.

## Дивіться Також:
- Microsoft Docs про `Invoke-RestMethod`: [link](https://docs.microsoft.com/en-us/powershell/module/microsoft.powershell.utility/invoke-restmethod)
- Про базову автентифікацію: [link](https://en.wikipedia.org/wiki/Basic_access_authentication)
