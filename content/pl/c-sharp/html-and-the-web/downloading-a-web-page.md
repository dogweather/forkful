---
date: 2024-01-20 17:43:31.177875-07:00
description: "Pobieranie strony internetowej to proces zapisywania jej zawarto\u015B\
  ci na dysku twardym. Programi\u015Bci robi\u0105 to, aby przetworzy\u0107 dane,\
  \ przeprowadzi\u0107 analiz\u0119\u2026"
lastmod: '2024-03-11T00:14:08.590108-06:00'
model: gpt-4-1106-preview
summary: "Pobieranie strony internetowej to proces zapisywania jej zawarto\u015Bci\
  \ na dysku twardym. Programi\u015Bci robi\u0105 to, aby przetworzy\u0107 dane, przeprowadzi\u0107\
  \ analiz\u0119\u2026"
title: Pobieranie strony internetowej
---

{{< edit_this_page >}}

## Co i Dlaczego?
Pobieranie strony internetowej to proces zapisywania jej zawartości na dysku twardym. Programiści robią to, aby przetworzyć dane, przeprowadzić analizę treści lub utworzyć kopię zapasową.

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
