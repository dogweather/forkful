---
title:                "Надсилання http-запиту з основною аутентифікацією."
html_title:           "PowerShell: Надсилання http-запиту з основною аутентифікацією."
simple_title:         "Надсилання http-запиту з основною аутентифікацією."
programming_language: "PowerShell"
category:             "PowerShell"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/powershell/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## Що і чому?

Відправлення HTTP-запиту з базовою автентифікацією - це процес взаємодії між програмістами та сховищем даних за допомогою протоколу HTTP з додатковим рівнем захисту. Коли програми отримують доступ до віддаленого сховища, вони повинні підтвердити свою ідентичність, щоб забезпечити безпеку обміну даними.

## Як це зробити:

```PowerShell
# Встановлення змінних для базової автентифікації
$username = "ім'я користувача"
$password = "пароль"

# Код для створення нового HTTP-запиту з базовою автентифікацією
$request = [System.Net.WebRequest]::Create("URL")
$request.Method = "GET"
$request.Credentials = New-Object System.Net.NetworkCredential($username, $password)
$response = $request.GetResponse()
```

Результат:

```
StatusCode          : 200
StatusDescription   : OK
ContentLength       : 1234
ContentType         : text/html; charset=utf-8
```


## Глибоке погруження:

1. Історичний контекст: у перших версіях протоколу HTTP автентифікація не була передбачена, але з часом було розроблено кілька способів захисту даних, в тому числі базову автентифікацію.

2. Альтернативи: існують інші методи автентифікації, такі як шифрування SSL, Token автентифікація або OAuth.

3. Деталі реалізації: для того, щоб передати базову автентифікацію в HTTP-запиті, необхідно встановити заголовок Authorization з Base64-кодированным ім'ям користувача та паролем. Важливо також враховувати, що базова автентифікація не є надійним методом захисту даних і може бути легко розшифрована зловмисником.

## Дивіться також:

- Повне посібник з HTTP-запитів з базовою автентифікацією в PowerShell: [посилання](https://docs.microsoft.com/en-us/powershell/scripting/samples/working-with-webrequests-from-windows-powershell-with-basic-authentication)
- Додаткова інформація про HTTP-запити та безпеку даних: [посилання](https://www.oreilly.com/library/view/secure-programming-cookbook/0596003943/ch08s14.html)