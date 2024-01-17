---
title:                "Analysera html"
html_title:           "Kotlin: Analysera html"
simple_title:         "Analysera html"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/kotlin/parsing-html.md"
---

{{< edit_this_page >}}

## Vad & Varför?

Parsing HTML handlar om att extrahera data från HTML-dokumentet och konvertera den till ett läsbart format för datorer. Detta är viktigt eftersom nästan all information på webben presenteras i HTML-format, så som titlar, text, bilder och länkar. Genom att använda parsing-tekniker kan programmerare enkelt hämta och använda all denna information för att skapa användbar mjukvara.

## Så här gör du:

```kotlin 
val input = "<h1>Hej världen!</h1>"
val output = jsoup.parse(input)
println(output.text())

// Output: Hej världen!
```

Det finns flera sätt att använda för att parsiva HTML i Kotlin, men en av de vanligaste är att använda biblioteket JSoup. Detta bibliotek tillåter dig att hämta data från en HTML-källa och manipulera det på olika sätt. I detta exempel parsar vi en enkel HTML-tagg och extraherar dess textinnehåll.

## Djupdykning:

Parsing HTML har varit en viktig del av webbutveckling sedan internet introducerades. Innan fanns det olika program som användes för att ladda ner och analysera HTML, men med tiden har mer sofistikerade tekniker utvecklats för att göra denna process snabbare och mer effektiv.

Som en alternativ metod för att parsiva HTML, kan programmerare också använda sig av regular expressions (regex) för att hitta och extrahera information från HTML-dokument. Men det finns risk för komplexa regex-uttryck och fel som kan uppstå från ändringar i HTML-strukturen. JSoup erbjuder ett mer tillförlitligt och enkelt sätt att parsiva HTML.

När det gäller implementationen av parsing HTML i Kotlin, finns det flera bibliotek att välja mellan. Utöver JSoup finns bibliotek som Kanna och Html-kit som också erbjuder olika funktioner för att parsiva HTML i Kotlin.

## Se även:

- [JSoup - ett HTML-parserbibliotek för Java och Kotlin](https://jsoup.org/)
- [Kanna - ett Kotlin-bibliotek för webbskrapning och parsing](https://github.com/Kotlin/kotshi)
- [Html-kit - parsning av HTML i Kotlin och Java](https://html-kit.github.io/)