---
title:                "Wysyłanie żądania http z podstawowym uwierzytelnieniem"
html_title:           "Arduino: Wysyłanie żądania http z podstawowym uwierzytelnieniem"
simple_title:         "Wysyłanie żądania http z podstawowym uwierzytelnieniem"
programming_language: "C#"
category:             "C#"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/c-sharp/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## Co to i Dlaczego?

Wysyłanie żądania HTTP z podstawowym uwierzytelnieniem to technika używana do autentykacji użytkownika w protokole HTTP za pomocą loginu i hasła. Programiści korzystają z tego, aby zapewnić bezpieczną komunikację i dostęp do chronionych zasobów.

## Jak to zrobić:

Przykładowy kod do wysłania żądania HTTP z podstawowym uwierzytelnieniem w języku C#:

```C#
using System;
using System.Net;
using System.Text;

public class Example
{
    static void Main()
    {
        try
        {
            WebClient client = new WebClient();
            string authInfo = "username:password";
            authInfo = Convert.ToBase64String(Encoding.Default.GetBytes(authInfo));
            client.Headers["Authorization"] = "Basic " + authInfo;

            string result = client.DownloadString("http://example.com");
            Console.WriteLine(result);
        }
        catch (Exception e)
        {
            Console.WriteLine(e.Message);
        }
    }
}
```

## Więcej informacji:

Historia:
Podstawowe uwierzytelnienie HTTP jest jednym z najstarszych standardów uwierzytelniania w protokole HTTP i pochodzi z wczesnych dni Internetu.

Alternatywy:
Nowsze formy uwierzytelniania webowej, takie jak uwierzytelnianie typu bearer (Bearer Authentication) lub uwierzytelnianie Digest, są uważane za bezpieczniejsze i bardziej niezawodne.

Szczegóły implementacji:
Podstawowe uwierzytelnienie HTTP polega na przesyłaniu nieszyfrowanych danych (użytkownik/hasło) w nagłówku HTTP. Z tego powodu, stosowane jest wyłącznie na pewnych, bezpiecznych połączeniach (np. połączeniach TLS).

## Zobacz też:

Wysyłanie żądań HTTP w C# - https://docs.microsoft.com/pl-pl/dotnet/api/system.net.http.httpclient?view=net-5.0

Uwierzytelnianie typu bearer - https://tools.ietf.org/html/rfc6750

Uwierzytelnianie Digest - https://tools.ietf.org/html/rfc7616

Bezpieczeństwo protokołu HTTP - https://tools.ietf.org/html/rfc7540#section-10.2