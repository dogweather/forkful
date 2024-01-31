---
title:                "Hämta en webbsida"
date:                  2024-01-20T17:43:50.851285-07:00
model:                 gpt-4-1106-preview
simple_title:         "Hämta en webbsida"

category:             "C#"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/c-sharp/downloading-a-web-page.md"
---

{{< edit_this_page >}}

## Vad & Varför?
Att ladda ner en webbsida innebär att hämta dess innehåll över internet. Programmerare gör detta för att extrahera data, automatisera testning av webbsidor eller för att påbörja webbskrapning.

## How to:
Använd `HttpClient` för att begära och få en webbsidas innehåll. Se exempel nedan:

```C#
using System;
using System.Net.Http;
using System.Threading.Tasks;

class Program
{
    static async Task Main()
    {
        using (HttpClient client = new HttpClient())
        {
            try
            {
                string url = "https://example.com";
                string responseBody = await client.GetStringAsync(url);
                Console.WriteLine(responseBody);
            }
            catch (HttpRequestException e)
            {
                Console.WriteLine("\nException Caught!");
                Console.WriteLine("Message :{0} ", e.Message);
            }
        }
    }
}
```

Exempel utskrift (förkortat för tydlighet):
```
<!doctype html>
<html>
<head>
    <title>Example Domain</title>
...
```

## Deep Dive
Historiskt sett har `WebClient` och `HttpWebRequest` använts i C# för att ladda ner webbsidor, men `HttpClient` är nu det moderna valet. Med `HttpClient`, kan du även dra nytta av asynkron programmering vilket gör din applikation mer responsiv. För alternativ, tänk på tredjepartsbibliotek som `HtmlAgilityPack` för att hantera HTML-parsing, om du behöver mer än bara det råa innehållet.

## See Also
- Microsoft Docs om `HttpClient`: https://docs.microsoft.com/en-us/dotnet/api/system.net.http.httpclient
- Tutorial om asynkron programmering i C#: https://docs.microsoft.com/en-us/dotnet/csharp/programming-guide/concepts/async/
- HtmlAgilityPack på GitHub: https://github.com/zzzprojects/html-agility-pack
