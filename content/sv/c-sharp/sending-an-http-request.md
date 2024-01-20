---
title:                "Skicka en http-förfrågan"
html_title:           "Javascript: Skicka en http-förfrågan"
simple_title:         "Skicka en http-förfrågan"
programming_language: "C#"
category:             "C#"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/c-sharp/sending-an-http-request.md"
---

{{< edit_this_page >}}

# Skicka HTTP-begäran i C#

## Vad & Varför?
Skicka HTTP-begäran låter programmet prata med webbservrar och API:er. Programmerare gör detta för att läsa/skriva data från/till externa källor.

## Hur man gör:
Använd `HttpClient` klassen i .NET-biblioteket. Här är en grundläggande POST-anrop med JSON-data.

```C#
using System;
using System.Net.Http;
using System.Text;
using System.Threading.Tasks;

var httpClient = new HttpClient();
var content = new StringContent("{ \"name\":\"John\"}", Encoding.UTF8, "application/json");

var response = await httpClient.PostAsync("https://api.sample.com/items", content);

if(response.IsSuccessStatusCode)
{
    var responseContent = await response.Content.ReadAsStringAsync();
    Console.WriteLine($"Response: {responseContent}");
}
```

## Djupdykning
1. **Historisk kontext**: Historiskt sett användes `WebClient` i .NET, men `HttpClient` är nu föredragen.
2. **Alternativ**: För mer komplexa fall, överväg `RestSharp` eller `Flurl.Http`.
3. **Implementeringsdetaljer**:
   - `HttpClient` bör återanvändas för flera begäran.
   - Hantera alltid fel med `try-catch` block.

## Se Även
- [MSDN HttpClient Dokument](https://docs.microsoft.com/en-us/dotnet/api/system.net.http.httpclient): Officiell `HttpClient` dokumentation.
- [RestSharp](http://restsharp.org): Alternativ C#-bibliotek.