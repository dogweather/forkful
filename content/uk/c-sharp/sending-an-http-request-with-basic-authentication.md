---
title:                "C#: Надсилання http-запиту з базовою автентифікацією"
simple_title:         "Надсилання http-запиту з базовою автентифікацією"
programming_language: "C#"
category:             "C#"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/c-sharp/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## Чому

Надсилання HTTP-запиту з базовою аутентифікацією є важливою задачею при роботі з мережевими додатками. Вона дозволяє забезпечити безпеку та доступ до ресурсів з використанням ідентифікаційних даних.

## Як

```C#
public class BasicAuthentication
{
    static readonly string username = "username";
    static readonly string password = "password";
    static readonly string apiUrl = "https://example.com/api/resource";

    public async Task MakeRequest()
    {
        var client = new HttpClient();
        var bytes = Encoding.ASCII.GetBytes($"{username}:{password}");
        var base64 = Convert.ToBase64String(bytes);
        client.DefaultRequestHeaders.Authorization = new AuthenticationHeaderValue("Basic", base64);
        var response = await client.GetAsync(apiUrl);

        Console.WriteLine("Response Code: " + response.StatusCode);
        Console.WriteLine("Response Content: " + await response.Content.ReadAsStringAsync());
    }
}
```

Приклад надсилання HTTP-запиту з базовою аутентифікацією використовуючи клас HttpClient та додавання заголовка Authorization з необхідними ідентифікаційними даними.

Вивід:

```
Response Code: 200
Response Content: Success!
```

## Глибше

Базова аутентифікація є одним зі способів аутентифікації у HTTP-протоколі. Вона передбачає передачу ідентифікаційних даних у заголовку Authorization, який складається з імені користувача та пароля, закодованих в форматі Base64. Приклад використання цього способу аутентифікації - стандартний HTTP-запит до API сервісу, який вимагає авторизації.

## Див. також

- [Повна документація по базовій аутентифікації в HTTP](https://developer.mozilla.org/en-US/docs/Web/HTTP/Authentication)
- [Документація по класу HttpClient в C#](https://docs.microsoft.com/en-us/dotnet/api/system.net.http.httpclient?view=net-5.0)