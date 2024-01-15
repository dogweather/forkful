---
title:                "Sända en http-begäran"
html_title:           "Elixir: Sända en http-begäran"
simple_title:         "Sända en http-begäran"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/elixir/sending-an-http-request.md"
---

{{< edit_this_page >}}

## Varför
Att skicka HTTP-förfrågningar är en viktig del av många mjukvaruprojekt, oavsett om det är för att hämta data från en API eller för att interagera med en webbplats. Med hjälp av Elixir blir det enklare och mer effektivt att hantera dessa förfrågningar, vilket är anledningen till att många utvecklare väljer detta språk.

## Så här gör du
Du behöver först installera Elixir på din dator. Sedan kan du enkelt skicka en HTTP-förfrågan genom att följa dessa steg:

```Elixir
httpc.request(:get, "https://www.example.com")
```

Det här är ett exempel på hur man kan skicka en GET-förfrågan till en URL. Det finns också andra parametrar som man kan använda för att skräddarsy förfrågningen, som till exempel HTTP-header och body. Läs mer om dessa i [Elixirs dokumentation](https://hexdocs.pm/elixir/httpc.html).

När du skickar en HTTP-förfrågan i Elixir, returneras en tuple med två värden: statuskoden för förfrågan och den faktiska data som har hämtats. Till exempel:

```Elixir
{200, "Det här är resultatet av min förfrågan"}
```

För att få ut specifik information, som till exempel bara HTTP-header eller body, kan du använda hjälpfunktioner som `:split_response` och `:fetch_body`.

## Djupdykning
En av de största fördelarna med att använda Elixir för att skicka HTTP-förfrågningar är att språket har fantastisk hantering av parallellism och konkurrens. Det betyder att man kan skicka flera förfrågningar samtidigt utan att behöva vänta på att en förfrågan ska slutföras innan man börjar på nästa.

Ett annat användbart verktyg för att hantera HTTP-förfrågningar är tesverktyget [hackney](https://github.com/benoitc/hackney). Detta erbjuder mer avancerade funktioner för att skicka förfrågningar, som att kunna sätta en timeout eller skicka förfrågningar med olika protokoll (t.ex. HTTPS).

## Se även
- [Elixirs dokumentation om HTTP-förfrågningar](https://hexdocs.pm/elixir/httpc.html)
- [Hackneys officiella hemsida](https://github.com/benoitc/hackney)