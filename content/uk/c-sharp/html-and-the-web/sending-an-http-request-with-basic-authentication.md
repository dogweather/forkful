---
title:                "Надсилання HTTP-запиту з базовою автентифікацією"
aliases:
- /uk/c-sharp/sending-an-http-request-with-basic-authentication.md
date:                  2024-01-20T18:01:26.795314-07:00
model:                 gpt-4-1106-preview
simple_title:         "Надсилання HTTP-запиту з базовою автентифікацією"

tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/c-sharp/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## Що це таке і навіщо?
Відправка HTTP-запиту з базовою аутентифікацією — це коли твоя програма мусить "представитись" серверу, щоб отримати доступ то потрібних даних. Програмісти це використовують для доступу до захищених ресурсів чи API.

## Як це зробити:
```C#
using System;
using System.Net.Http;
using System.Text;
using System.Threading.Tasks;

class Program
{
    static async Task Main()
    {
        var username = "user";
        var password = "pass";
        var url = "http://example.com/api";

        using var client = new HttpClient();
        var authToken = Encoding.ASCII.GetBytes($"{username}:{password}");
        client.DefaultRequestHeaders.Authorization = new System.Net.Http.Headers.AuthenticationHeaderValue("Basic", Convert.ToBase64String(authToken));

        var response = await client.GetAsync(url);
        
        if (response.IsSuccessStatusCode)
        {
            string responseBody = await response.Content.ReadAsStringAsync();
            Console.WriteLine(responseBody);
        }
        else
        {
            Console.WriteLine($"Error: {response.StatusCode}");
        }
    }
}
```
Output:
```
{"data":"Це якийсь захищений вміст..."}
```

## Докладніше:
Базова аутентифікація – це старий, але простий метод для захисту даних. Вона кодує логін і пароль в Base64 і включає це в заголовок запиту. Але оскільки Base64 – це не шифрування, ніколи не використовуй її без HTTPS. Інакше, секретні дані можуть бути зламані під час передачі.

Альтернативи базовій аутентифікації включають OAuth, токени сесій, JWT (JSON Web Tokens) тощо. В цих методах, посвідчення користувача перевіряються один раз, а потім сервер надає токен, який використовується для подальшої ідентифікації.

## Також дивіться:
- Документація Microsoft по HttpClient: https://docs.microsoft.com/dotnet/api/system.net.http.httpclient
- Базова аутентифікація на MDN: https://developer.mozilla.org/en-US/docs/Web/HTTP/Authentication
- Про безпечніше альтернативне аутентифікування: https://auth0.com/learn/token-based-authentication-made-easy/
- Як налаштувати HTTPS у .NET: https://docs.microsoft.com/en-us/aspnet/core/security/enforcing-ssl
