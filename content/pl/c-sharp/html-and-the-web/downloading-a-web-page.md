---
date: 2024-01-20 17:43:31.177875-07:00
description: "Jak to zrobi\u0107: W C# pobranie strony webowej jest proste. U\u017C\
  ywamy `HttpClient`, a oto przyk\u0142ad."
lastmod: '2024-03-13T22:44:35.407235-06:00'
model: gpt-4-1106-preview
summary: W C# pobranie strony webowej jest proste.
title: Pobieranie strony internetowej
weight: 42
---

## Jak to zrobić:
W C# pobranie strony webowej jest proste. Używamy `HttpClient`, a oto przykład:

```C#
using System;
using System.Net.Http;
using System.Threading.Tasks;

class Program
{
    static async Task Main(string[] args)
    {
        using (HttpClient client = new HttpClient())
        {
            HttpResponseMessage response = await client.GetAsync("http://example.com");
            response.EnsureSuccessStatusCode();
            string responseBody = await response.Content.ReadAsStringAsync();
            Console.WriteLine(responseBody);
        }
    }
}
```

Wykonanie powyższego programu wyświetli HTML pobranej strony.

## Głębsze spojrzenie:
W przeszłości używano `WebClient` lub `HttpWebRequest`, ale `HttpClient` jest nowocześniejszy, wydajniejszy i lepiej przystosowany do asynchronicznego programowania. Istnieje kilka alternatyw, takich jak używanie bibliotek zewnętrznych (np. HtmlAgilityPack) do konkretnych zastosowań, jak web scraping. Implementacja `HttpClient` wspiera asynchroniczność od samego początku, co jest kluczowe przy operacjach sieciowych, pozwalając na lepsze zarządzanie zasobami, szczególnie przy dużym obciążeniu.

## Zobacz również:
- Oficjalna dokumentacja `HttpClient`: https://docs.microsoft.com/en-us/dotnet/api/system.net.http.httpclient
- Porównanie `HttpClient` z `WebClient`: https://www.c-sharpcorner.com/article/httpclient-vs-webclient-vs-httpwebrequest/
- HTML Agility Pack, biblioteka do web scrapingu: https://html-agility-pack.net/
