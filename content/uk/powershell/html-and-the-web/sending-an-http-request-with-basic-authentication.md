---
title:                "Надсилання HTTP-запиту з базовою автентифікацією"
aliases:
- /uk/powershell/sending-an-http-request-with-basic-authentication/
date:                  2024-01-20T18:02:45.339018-07:00
model:                 gpt-4-1106-preview
simple_title:         "Надсилання HTTP-запиту з базовою автентифікацією"

tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/powershell/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## Що та Навіщо?
Відправлення HTTP запиту з базовою автентифікацією означає, що користувач передає логін і пароль для доступу до ресурсу. Програмісти роблять це, щоби інтегруватися з веб-сервісами, які потребують авторизації.

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
