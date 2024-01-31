---
title:                "Een webpagina downloaden"
date:                  2024-01-28T21:59:19.298344-07:00
model:                 gpt-4-0125-preview
simple_title:         "Een webpagina downloaden"

category:             "Java"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/nl/java/downloading-a-web-page.md"
changelog:
  - 2024-01-28, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Wat & Waarom?

Een webpagina downloaden betekent het programmatisch grijpen van de inhoud, zoals HTML, CSS en JavaScript. Programmeurs doen dit om gegevens te verwerken, wijzigingen te monitoren of hun webapps te testen.

## Hoe te:

```java
import java.io.BufferedReader;
import java.io.InputStreamReader;
import java.net.URL;

public class WebPageDownloader {
    public static void main(String[] args) {
        String urlStr = "http://voorbeeld.com";
        probeer {
            URL url = new URL(urlStr);
            probeer (BufferedReader reader = new BufferedReader(new InputStreamReader(url.openStream()))) {
                String regel;
                terwijl ((regel = reader.readLine()) != null) {
                    System.out.println(regel);
                }
            }
        } vang (Exception e) {
            e.printStackTrace();
        }
    }
}
```

Een voorbeelduitvoer ziet er als volgt uit:

```
<!doctype html>
<html>
<head>
    <title>Voorbeeld Domein</title>
    ...
</head>
...
</html>
```

## Diepgaande duik

Vroeger was het downloaden van een webpagina kinderspel - HTTP was eenvoudig, websites waren voornamelijk statische HTML. Het web van vandaag is complex - denk aan HTTPS, door JavaScript aangedreven inhoud en AJAX in overvloed.

Voor statische inhoud zijn `java.net.URL` en `java.net.HttpURLConnection` de voor de hand liggende keuzes - zonder poespas, werkt gewoon. Maar als je gericht bent op sites vol met dynamische inhoud geladen door JavaScript, zullen die klassen alleen niet volstaan, en kijk je naar hulpmiddelen als Selenium of HtmlUnit in plaats daarvan.

Vergeet niet, het kiezen van het juiste hulpmiddel hangt ook af van wat je moet doen met de pagina zodra deze is gedownload. HTML parsen? Jsoup is je beste vriend. JavaScript uitvoeren? Overweeg een headless browser. De `java.net` klassen zijn slechts het topje van de ijsberg, maar ze zijn geschikt voor snelle taken of gegevens schrapen van gewone oude webpagina's.

Onthoud het beleid van beleefdheid: bestook een site niet met snelle verzoeken, anders vraag je om een verbanning. En zorg ervoor dat je netjes omgaat met de `robots.txt` richtlijnen van de website.

## Zie ook

- De [Jsoup bibliotheek](https://jsoup.org/) voor HTML-parsing en -extractie.
- De [Selenium WebDriver](https://www.selenium.dev/documentation/en/webdriver/) voor complexere taken inclusief JavaScript-uitvoering.
- Een gids voor [HttpURLConnection](https://docs.oracle.com/javase/8/docs/api/java/net/HttpURLConnection.html) voor degenen die de details willen weten over Java's ingebouwde manier om HTTP te hanteren.
- [HtmlUnit](http://htmlunit.sourceforge.net/), een "GUI-loze browser voor Java-programma's", geweldig voor pagina's zwaar beladen met JavaScript.
