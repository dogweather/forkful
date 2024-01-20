---
title:                "Надсилаємо HTTP-запит з базової аутентифікацією"
html_title:           "C#: Надсилаємо HTTP-запит з базової аутентифікацією"
simple_title:         "Надсилаємо HTTP-запит з базової аутентифікацією"
programming_language: "PowerShell"
category:             "PowerShell"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/powershell/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## Що та навіщо?

Відправлення HTTP-запиту з базовою аутентифікацією — це процес обміну даними між клієнтом та сервером, де ідентичність вичерпно перевіряється. Програмісти роблять це для забезпечення безпеки даних та захисту від несанкціонованого доступу.

## Як це зробити:

```PowerShell
#Встановлюємо ім'я користувача та пароль
$userName = 'Your UserName'
$password = 'Your Password'

#Кодуємо в Base64
$base64AuthInfo = [Convert]::ToBase64String([Text.Encoding]::ASCII.GetBytes(("{0}:{1}" -f $userName,$password)))

#Create HTTP Request
$httpRequest = New-Object System.Net.HttpWebRequest
$httpRequest.Method = 'POST'
$httpRequest.Headers.Add('Authorization',('Basic {0}' -f $base64AuthInfo))

#Відправляємо запит та отримуємо відповідь
$response = $httpRequest.GetResponse()
$status = $response.StatusCode
$responseData = new-object System.IO.StreamReader $response.GetResponseStream()
$responseData.ReadToEnd()
```

## Поглиблений аналіз

1. Історичний контекст: Базова аутентифікація є одним з найперших методів аутентифікації, впроваджених в HTTP протоколі. Її простота і легкість впровадження є причиною її широкого застосування.
 
2. Альтернативи: Kerberos, Digest же і OAuth це декілька з багатьох доступних альтернатив. Вони більш безпечні, але вимагають більш складної конфігурації. 

3. Деталі імплементації: Імена користувачів та паролі кодуються ще перед надсиланням до сервера. Це підвищує безпеку, але не робить процес абсолютно недоступним для вторгнень.

## Дивіться також

[Official Documentation](https://docs.microsoft.com/en-us/powershell/scripting/overview?view=powershell-7.1)
[Basic Authentication](https://developer.mozilla.org/en-US/docs/Web/HTTP/Authentication)
[PowerShell HTTP Programming](https://www.computerperformance.co.uk/powershell/powershell-invoke-webrequest/)