---
title:                "Ladda ner en webbsida"
html_title:           "Kotlin: Ladda ner en webbsida"
simple_title:         "Ladda ner en webbsida"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/kotlin/downloading-a-web-page.md"
---

{{< edit_this_page >}}

## Varför

Vi lever i en digital era där internet är en integrerad del av våra liv. Att ladda ner en webbsida kan vara användbart för att spara information, tillgång till offline versioner av sidor, eller för att enkelt göra en backup av viktig information.

## Så här gör du

För att ladda ner en webbsida i Kotlin, behöver du använda dig av klassen `URL` och funktionen `readText()`:

```Kotlin
val url = URL("https://example.com")
val pageContent = url.readText()
```

I detta exempel skapar vi en `URL`-objekt som representerar webbadressen till sidan vi vill ladda ner. Sedan använder vi funktionen `readText()` för att läsa all text på sidan och spara den i variabeln `pageContent`.

För att sedan spara ned den nerladdade sidan som en fil, kan vi använda oss av `FileWriter`-klassen:

```Kotlin
// Skapa en ny fil
val file = File("webbsida.html")

// Skapa en FileWriter för att skriva till filen
val writer = FileWriter(file)

// Skriv nerladdad data till filen
writer.write(pageContent)

// Stäng FileWriter
writer.close()
```

Nu har vi sparat den nerladdade webbsidan som en fil på vår dator.

## Djupdykning

När vi laddar ner en webbsida i Kotlin, använder vi oss av URL-klassen för att representera webbadressen och `readText()`-funktionen för att läsa all text på sidan. Detta fungerar bra för mindre sidor, men om sidan är större kan det finnas risker för minnesläckor eller att sidan inte laddas ner helt.

För att undvika detta, kan vi använda oss av en annan metod för att läsa sidans innehåll - `openStream()`. Denna metod ger oss tillgång till en instans av `InputStream`, som vi sedan kan läsa data från en bit i taget. Detta minskar risken för minnesläckor och lägger mindre belastning på vårt system.

En annan sak att tänka på när man laddar ner webbsidor är att de kan innehålla bilder och andra medieelement. För att också kunna ladda ner dessa, kan vi använda oss av bibliotek som `Jsoup` eller `OkHttp` som erbjuder mer avancerade funktioner för nedladdning av webbsidor.

## Se även

- [Kotlin Dokumentation](https://kotlinlang.org/docs/)
- [Jsoup biblioteket](https://jsoup.org/)
- [OkHttp biblioteket](https://square.github.io/okhttp/)