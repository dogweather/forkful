---
title:                "Отправка HTTP-запроса с базовой аутентификацией"
date:                  2024-01-29T00:02:42.869404-07:00
model:                 gpt-4-0125-preview
simple_title:         "Отправка HTTP-запроса с базовой аутентификацией"
programming_language: "C#"
category:             "C#"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ru/c-sharp/sending-an-http-request-with-basic-authentication.md"
changelog:
  - 2024-01-29, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Что и почему?
Мы отправляем HTTP-запрос с базовой аутентификацией для доступа к защищенным ресурсам, включая учетные данные пользователя в заголовок запроса. Программисты используют это для простых систем аутентификации, главным образом, когда подходит быстрое и простое решение.

## Как это сделать:
Давайте сразу перейдем к коду. Ниже приведен минимальный пример использования C# для отправки HTTP-запроса с базовой аутентификацией:

```C#
using System;
using System.Net.Http;
using System.Net.Http.Headers;
using System.Text;
using System.Threading.Tasks;

class Program
{
    static async Task Main()
    {
        using (var client = new HttpClient())
        {
            var credentials = Convert.ToBase64String(Encoding.ASCII.GetBytes("username:password"));
            client.DefaultRequestHeaders.Authorization = new AuthenticationHeaderValue("Basic", credentials);

            HttpResponseMessage response = await client.GetAsync("http://yourapi.com/protected");

            if (response.IsSuccessStatusCode)
            {
                string responseBody = await response.Content.ReadAsStringAsync();
                Console.WriteLine(responseBody);
            }
            else
            {
                Console.WriteLine($"Ошибка: {response.StatusCode}");
            }
        }
    }
}
```
Запустите это, и если ваш конечный точка и учетные данные верны, вы получите ресурс. В противном случае вы увидите код состояния ошибки.

## Подробнее
Базовая аутентификация действительно очень старая, она появилась еще в ранние дни интернета. Это просто: кодируйте в base64 "username:password" и добавьте это в заголовок `Authorization`.

Есть альтернативы с более строгой безопасностью: OAuth2, API-ключи или токены JWT. Базовая аутентификация до сих пор используется из-за своей простоты, но имейте в виду, что она не шифруется и может быть перехвачена, если не использовать ее через HTTPS.

Когда вы используете этот метод, имейте в виду:
- Всегда используйте HTTPS для защиты учетных данных во время передачи.
- Это немного похоже на оставление ключа от дома под ковриком – удобно, но уязвимо. Поэтому используйте это для сценариев с низким риском.
- Поскольку учетные данные передаются с каждым запросом, это не самый эффективный метод для занятых систем.

## Смотрите также
- [Документация по классу HttpClient от Microsoft](https://docs.microsoft.com/en-us/dotnet/api/system.net.http.httpclient)
- [Объяснение базовой аутентификации от Mozilla](https://developer.mozilla.org/en-US/docs/Web/HTTP/Authentication)
- [Шпаргалка по аутентификации от OWASP](https://owasp.org/www-project-cheat-sheets/cheatsheets/Authentication_Cheat_Sheet.html)
