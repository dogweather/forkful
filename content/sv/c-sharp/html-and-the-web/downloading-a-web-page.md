---
date: 2024-01-20 17:43:50.851285-07:00
description: "Att ladda ner en webbsida inneb\xE4r att h\xE4mta dess inneh\xE5ll \xF6\
  ver internet. Programmerare g\xF6r detta f\xF6r att extrahera data, automatisera\
  \ testning av\u2026"
lastmod: '2024-03-13T22:44:37.912080-06:00'
model: gpt-4-1106-preview
summary: "Att ladda ner en webbsida inneb\xE4r att h\xE4mta dess inneh\xE5ll \xF6\
  ver internet. Programmerare g\xF6r detta f\xF6r att extrahera data, automatisera\
  \ testning av\u2026"
title: "H\xE4mta en webbsida"
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
