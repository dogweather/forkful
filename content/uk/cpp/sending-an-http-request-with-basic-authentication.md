---
title:                "Відправка запиту http з основною аутентифікацією"
html_title:           "C++: Відправка запиту http з основною аутентифікацією"
simple_title:         "Відправка запиту http з основною аутентифікацією"
programming_language: "C++"
category:             "C++"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/cpp/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

# Що & Чому?
Відправка HTTP запиту з основною аутентифікацією - це процес надсилання запиту до веб-сервера зі спеціальним кодом для ідентифікації себе як користувача. Це потрібно для того, щоб здійснити з'єднання з захищеним сервером і отримати доступ до захищених ресурсів. Програмісти використовують цей метод для збільшення безпеки комунікації з веб-сайтами.

# Як це зробити:
Приклад коду на C++, який надсилає HTTP запит з основною аутентифікацією і виводить результат запиту на екран:

```C++
#include <iostream>
#include <wininet.h>

using namespace std;

// Змінні для запиту
HINTERNET hInternet, hConnect, hRequest;
LPCTSTR lpszAgent = "WinHTTP Example/1.0";
LPCTSTR lpszServerName = "www.example.com";
LPCTSTR lpszUsername = "username";
LPCTSTR lpszPassword = "password";

// Будемо виводити вміст відповіді на консоль
cout << "The response is: " << endl;

hInternet = ::InternetOpen(lpszAgent, INTERNET_OPEN_TYPE_PRECONFIG, NULL, NULL, 0);

// Підключаємося до сервера
hConnect = ::InternetConnect(hInternet, lpszServerName,
INTERNET_DEFAULT_HTTP_PORT, lpszUsername, lpszPassword, INTERNET_SERVICE_HTTP, 0, NULL);

// Створюємо HTTP запит
hRequest = ::HttpOpenRequest(hConnect, "GET", "/", NULL, 0, 0, INTERNET_FLAG_KEEP_CONNECTION, 0);

BOOL bRequestSent = ::HttpSendRequest(hRequest, NULL, 0, NULL, 0);

if (!bRequestSent)
{
    cout << "Failed to send HTTP request." << endl;
    return 0;
}

DWORD dwNumberOfBytesRead = 0;
BOOL bResult = FALSE;
CHAR sBuffer[1024] = {0};

// Зчитуємо вміст відповіді та виводимо його на екран
bResult = ::InternetReadFile(hRequest, sBuffer, sizeof(sBuffer) - 1, &dwNumberOfBytesRead);
cout << sBuffer << endl;

// Закриваємо з'єднання та очищаємо пам'ять
::InternetCloseHandle(hRequest);
::InternetCloseHandle(hConnect);
::InternetCloseHandle(hInternet);
```

Результат запиту:
```
<!doctype html>
<html>
<head>
    <title>Welcome to Example.com</title>
</head>
<body>
    <h1>Welcome!</h1>
</body>
</html>
```

# Глибше вдивимося:
(1) Основна аутентифікація була введена у специфікації HTTP 1.0. Вона передбачає відправку ім'я користувача та пароля у заголовках запиту, що може бути не безпечним для використання. Альтернативою є аутентифікація на основі токена, яка передбачає надсилання унікального токена замість логіну та пароля.

(2) У прикладі використовується бібліотека WinInet, яка надає функції для взаємодії з мережею. Також можна використовувати бібліотеку libcurl для надсилання HTTP запитів з основною аутентифікацією.

(3) Детальніше про основну аутентифікацію можна дізнатися у RFC 7617: https://tools.ietf.org/html/rfc7617

# Дивіться також:
* Приклад використання основної аутентифікації з бібліотекою libcurl на мові C: https://curl.haxx.se/libcurl/c/CURLOPT_USERNAME.html
* Огляд служби WinHTTP: https://docs.microsoft.com/en-us/windows/win32/winhttp/winhttp-start-page