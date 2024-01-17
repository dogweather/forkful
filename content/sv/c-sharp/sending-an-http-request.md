---
title:                "Sända en http-begäran"
html_title:           "C#: Sända en http-begäran"
simple_title:         "Sända en http-begäran"
programming_language: "C#"
category:             "C#"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/c-sharp/sending-an-http-request.md"
---

{{< edit_this_page >}}

## Vad och Varför?
Att skicka en HTTP-begäran (HTTP request) är en viktig del av programskapande för att interagera med webbserverar och få tillgång till webbsidor, data och andra resurser. Det är ett sätt för programmerare att kommunicera med andra datorer på internet och få svar tillbaka.

## Hur man gör:
```
C# public static async Task Main(string[] args)
{
    // Skapa en HttpClient-instans för att skicka HTTP-begäran
    HttpClient client = new HttpClient();

    // Ange den webbadress som begäran ska skickas till
    string address = "https://www.example.com";

    // Skicka begäran och vänta på svar
    HttpResponseMessage response = await client.GetAsync(address);

    // Hämta svaret som en sträng
    string result = await response.Content.ReadAsStringAsync();

    // Skriv ut resultatet
    Console.WriteLine(result);

    // Kom ihåg att stänga HTTP-klienten för att frigöra resurser
    client.Dispose();
}
```

Output:
```
<html>
<head>
<title>Example Domain</title>
</head>
<body>
<h1>Example Domain</h1>
<p>This domain is for use in illustrative examples in documents. 
You may use this
domain in literature without prior coordination or asking for permission.</p>
<p><a href="https://www.iana.org/domains/example">More information...</a></p>
</body>
</html>
```

## Deep Dive:
HTTP-begäran (Hypertext Transfer Protocol Request) har funnits sedan 1991 och är en del av det grundläggande protokollsystemet för internet. Det finns många olika metoder för att skapa och skicka HTTP-begäran, men i C# använder man oftast System.Net.Http.HttpClient-klassen för att hantera kommunikationen. Det finns också alternativ som t.ex. RestSharp eller WebClient som kan användas för att göra HTTP-begäran enklare. Implementationen av att skicka en HTTP-begäran kan vara komplex och det är viktigt att rätt åtgärder vidtas för att säkerställa en säker och pålitlig kommunikation med servern.

## See Also:
Läs mer om HTTP-begäran:
- [HTTP-begäran på MDN](https://developer.mozilla.org/en-US/docs/Web/HTTP/Methods)
- [HTTP-begäran på W3 Schools](https://www.w3schools.com/tags/ref_httpmethods.asp)
- [HTTP-begäran i C# på C# Corner](https://www.c-sharpcorner.com/article/httpclient-in-C-Sharp/)