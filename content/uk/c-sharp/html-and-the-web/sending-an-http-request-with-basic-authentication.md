---
date: 2024-01-20 18:01:26.795314-07:00
description: "\u0412\u0456\u0434\u043F\u0440\u0430\u0432\u043A\u0430 HTTP-\u0437\u0430\
  \u043F\u0438\u0442\u0443 \u0437 \u0431\u0430\u0437\u043E\u0432\u043E\u044E \u0430\
  \u0443\u0442\u0435\u043D\u0442\u0438\u0444\u0456\u043A\u0430\u0446\u0456\u0454\u044E\
  \ \u2014 \u0446\u0435 \u043A\u043E\u043B\u0438 \u0442\u0432\u043E\u044F \u043F\u0440\
  \u043E\u0433\u0440\u0430\u043C\u0430 \u043C\u0443\u0441\u0438\u0442\u044C \"\u043F\
  \u0440\u0435\u0434\u0441\u0442\u0430\u0432\u0438\u0442\u0438\u0441\u044C\" \u0441\
  \u0435\u0440\u0432\u0435\u0440\u0443, \u0449\u043E\u0431 \u043E\u0442\u0440\u0438\
  \u043C\u0430\u0442\u0438 \u0434\u043E\u0441\u0442\u0443\u043F \u0442\u043E \u043F\
  \u043E\u0442\u0440\u0456\u0431\u043D\u0438\u0445 \u0434\u0430\u043D\u0438\u0445\
  .\u2026"
lastmod: '2024-03-13T22:44:49.288529-06:00'
model: gpt-4-1106-preview
summary: "\u0412\u0456\u0434\u043F\u0440\u0430\u0432\u043A\u0430 HTTP-\u0437\u0430\
  \u043F\u0438\u0442\u0443 \u0437 \u0431\u0430\u0437\u043E\u0432\u043E\u044E \u0430\
  \u0443\u0442\u0435\u043D\u0442\u0438\u0444\u0456\u043A\u0430\u0446\u0456\u0454\u044E\
  \ \u2014 \u0446\u0435 \u043A\u043E\u043B\u0438 \u0442\u0432\u043E\u044F \u043F\u0440\
  \u043E\u0433\u0440\u0430\u043C\u0430 \u043C\u0443\u0441\u0438\u0442\u044C \"\u043F\
  \u0440\u0435\u0434\u0441\u0442\u0430\u0432\u0438\u0442\u0438\u0441\u044C\" \u0441\
  \u0435\u0440\u0432\u0435\u0440\u0443, \u0449\u043E\u0431 \u043E\u0442\u0440\u0438\
  \u043C\u0430\u0442\u0438 \u0434\u043E\u0441\u0442\u0443\u043F \u0442\u043E \u043F\
  \u043E\u0442\u0440\u0456\u0431\u043D\u0438\u0445 \u0434\u0430\u043D\u0438\u0445\
  .\u2026"
title: "\u041D\u0430\u0434\u0441\u0438\u043B\u0430\u043D\u043D\u044F HTTP-\u0437\u0430\
  \u043F\u0438\u0442\u0443 \u0437 \u0431\u0430\u0437\u043E\u0432\u043E\u044E \u0430\
  \u0432\u0442\u0435\u043D\u0442\u0438\u0444\u0456\u043A\u0430\u0446\u0456\u0454\u044E"
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
