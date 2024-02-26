---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 22:03:34.572124-07:00
description: "HTML parsen betekent graven door de opmaak om gegevens zoals tekst,\
  \ links of andere elementen te extraheren. We doen dit om te interageren met of\u2026"
lastmod: '2024-02-25T18:49:48.021628-07:00'
model: gpt-4-0125-preview
summary: "HTML parsen betekent graven door de opmaak om gegevens zoals tekst, links\
  \ of andere elementen te extraheren. We doen dit om te interageren met of\u2026"
title: HTML Parsen
---

{{< edit_this_page >}}

## Wat & Waarom?

HTML parsen betekent graven door de opmaak om gegevens zoals tekst, links of andere elementen te extraheren. We doen dit om te interageren met of webinhoud te schrapen, browsertaken te automatiseren of webapps te testen.

## Hoe:

Laten we Jsoup gebruiken, een handige bibliotheek voor het werken met echte HTML. Voeg eerst de afhankelijkheid toe:

```xml
<dependency>
    <groupId>org.jsoup</groupId>
    <artifactId>jsoup</artifactId>
    <version>1.15.2</version>
</dependency>
```

Nu naar het leuke deel. Hier is hoe je de titel van een webpagina kunt grijpen en printen:

```java
import org.jsoup.Jsoup;
import org.jsoup.nodes.Document;

public class HtmlParser {
    public static void main(String[] args) throws IOException {
        String url = "http://example.com";
        Document doc = Jsoup.connect(url).get();
        String title = doc.title();
        System.out.println("Titel: " + title);
    }
}
```

Uitvoer:

```
Titel: Voorbeeld Domein
```

Hoe zit het met het extraheren van alle links?

```java
import org.jsoup.nodes.Element;
import org.jsoup.select.Elements;

// ... binnen de main of een andere methode
Elements links = doc.select("a[href]");
for (Element link : links) {
    System.out.println("Link: " + link.attr("href"));
}
```

## Diepgaande duik

Ooit werd HTML getemd door regex patronen, een methode zowel foutgevoelig als nachtmerrieachtig voor complexe documenten. Toen kwam Jsoup in de late jaren nul, die een jQuery-achtige interface voor Java biedt om HTML te parsen, te doorlopen en te manipuleren.

Jsoup is niet de enige keuze. Er is HtmlUnit voor volwaardige webapptesten met JavaScript-ondersteuning, maar het is zwaarder en complexer. Voor lichtgewicht taken is Apache Commons Validator geweldig, alleen al voor het extraheren van URL's.

Onder de motorkap gebruikt Jsoup een DOM-parser, die het hele document in het geheugen modelleert als een boom. Deze aanpak maakt het selecteren en navigeren van de HTML-structuur een fluitje van een cent. Bovendien is het vergevingsgezind met slordige HTML, het lost problemen onderweg op om robuust parsen te garanderen.

Onthoud, bij het scrapen, altijd de `robots.txt` van een site controleren en de gebruiksvoorwaarden om juridische problemen of een IP-ban te vermijden.

## Zie ook

- Jsoup Officiële Documentatie: https://jsoup.org/
- HtmlUnit: http://htmlunit.sourceforge.net/
- Apache Commons Validator: https://commons.apache.org/proper/commons-validator/
