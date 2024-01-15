---
title:                "Sända en http förfrågan"
html_title:           "Haskell: Sända en http förfrågan"
simple_title:         "Sända en http förfrågan"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/haskell/sending-an-http-request.md"
---

{{< edit_this_page >}}

## Varför

Att utföra en HTTP-begäran är en viktig del av webbutveckling. Det tillåter ditt program att kommunicera med andra servrar och hämta data från olika källor.

## Så här gör du

Det enklaste sättet att utföra en HTTP-begäran i Haskell är genom att använda ett tredjepartsbibliotek som kallas "Wreq". Detta bibliotek ger en högnivå API för att göra HTTP-begäran och tolka svaren.

Först behöver du importera Wreq-modulen i ditt projekt:

```Haskell
import Network.Wreq
```

Sedan kan du använda funktionen "get" för att göra en enkel GET-begäran till en URL:

```Haskell
let response = get "https://example.com"
```

För att använda "get"-funktionen måste du också importera "Control.Lens" - en annan modul som tillhandahåller hjälpfunktioner för att manipulera datastrukturer.

För att hämta svaret från begäran kan du använda funktionen "^.responseBody":

```Haskell
let response = get "https://example.com"

print (response ^.responseBody)
```

Detta kommer att skriva ut den resulterande HTML-koden från begäran.

För att utföra andra typer av HTTP-begäran, såsom POST eller PUT, kan du använda funktionen "postWith" eller "putWith" och skicka med data som en andra parameter:

```Haskell
let response = postWith options "https://example.com" "{ \"name\": \"John\", \"age\": 27 }"
```

Denna kod skulle göra en POST-begäran med den angivna datan som kropp och svara med resultatet.

Det finns många andra funktioner och möjligheter med Wreq-biblioteket, så se till att utforska dokumentationen för mer information.

## Deep Dive

Wreq-biblioteket är en wrapper för "curl" biblioteket, vilket innebär att det använder "curl" under huven för att utföra HTTP-begäran. Detta ger stabilitet och pålitlighet till biblioteket, eftersom "curl" är mycket beprövat och används i många andra olika programmeringsspråk.

En annan fördel med att använda Wreq är att den har inbyggd hantering av JSON-svar. Om svaret från din begäran är i JSON-format, kommer Wreq att tolka det och göra det möjligt för dig att komma åt data enkelt.

Om du är intresserad av att lära dig mer om HTTP-begäran och kommunikation över webben, finns det många resurser tillgängliga online som kan hjälpa dig att förstå mer avancerade koncept och tekniker.

## Se även

- [Wreq dokumentation](https://hackage.haskell.org/package/wreq)
- [Haskell HTTP bibliotek](https://wiki.haskell.org/HTTP)
- [En introduktion till funktionell programmering med Haskell](https://www.codementor.io/@gabdulmajid/an-introduction-to-functional-programming-with-haskell-9241iz1ii)