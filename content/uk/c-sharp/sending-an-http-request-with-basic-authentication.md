---
title:                "Відправлення запиту http з базовою автентифікацією"
html_title:           "C#: Відправлення запиту http з базовою автентифікацією"
simple_title:         "Відправлення запиту http з базовою автентифікацією"
programming_language: "C#"
category:             "C#"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/c-sharp/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

Що & Чому?
Надсилання HTTP-запиту з базовою автентифікацією - це процес надсилання запиту до веб-сервера з використанням особистих ідентифікаторів для підтвердження користувача. Програмісти виконують це для забезпечення безпеки даних та захисту конфіденційності.

Як це зробити:
```C#
using System.Net.Http;

public static void SendRequest()
{
    Uri uri = new Uri("https://www.example.com/endpoint");
    HttpClient httpClient = new HttpClient();
    var byteArray = Encoding.ASCII.GetBytes("username:password");
    httpClient.DefaultRequestHeaders.Authorization = new System.Net.Http.Headers.AuthenticationHeaderValue("Basic", Convert.ToBase64String(byteArray));
    HttpResponseMessage response = httpClient.GetAsync(uri).GetAwaiter().GetResult();
    Console.WriteLine(response.StatusCode);
}
```

Глибокий погляд:
Надсилання HTTP-запиту з базовою автентифікацією було спроектоване для забезпечення безпеки передачі даних між клієнтом і сервером. Альтернативою цьому є використання токенів або ідентифікаційних ключів. Щоб реалізувати надсилання запиту з базовою автентифікацією, потрібно скласти кодування особистих ідентифікаторів та додати його до заголовків HTTP-запиту.

Див. також:
- Документація Microsoft про використання базової автентифікації: https://docs.microsoft.com/en-us/dotnet/api/system.net.http.httpclient?view=netcore-3.1