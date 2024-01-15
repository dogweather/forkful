---
title:                "Hämta en webbsida"
html_title:           "Java: Hämta en webbsida"
simple_title:         "Hämta en webbsida"
programming_language: "Java"
category:             "Java"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/java/downloading-a-web-page.md"
---

{{< edit_this_page >}}

## Varför

Att ladda ner en webbsida kan vara en användbar funktion när du vill spara en kopia av en sida för senare referens eller när du arbetar med web skrapning och datainsamling.

## Hur man gör det

För att ladda ner en webbsida i Java, kan du använda klassen URL och dess openStream()-metod. Nedan är ett exempel på hur du kan implementera detta:

```Java
import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStreamReader;
import java.net.URL;

public class WebPageDownloader {

    public static void main(String[] args) throws IOException {

        // ange webbadress som ska laddas ner
        String url = "https://www.example.com";

        // skapa en URL-objekt
        URL myURL = new URL(url);

        // öppna en anslutning till webbadressen och ladda ner innehållet som en BufferedReader
        BufferedReader in = new BufferedReader(new InputStreamReader(myURL.openStream()));

        // använd en StringBuilder för att lagra innehållet från webbsidan
        StringBuilder sb = new StringBuilder();
        String line;

        // läs rad för rad från BufferedReader och lagra i StringBuilder
        while ((line = in.readLine()) != null) {
            sb.append(line + "\n");
        }
        in.close();

        // skriv ut innehållet från StringBuilder
        System.out.println(sb.toString());

    }
}
```

Detta kommer att skriva ut innehållet från den angivna webbadressen till konsolen. Du kan också välja att spara innehållet till en fil istället för att skriva ut det.

## Djupdykning

När du laddar ner en webbsida, kommer du att hämta innehållet som visas på sidan när du besöker den med en webbläsare. Detta inkluderar HTML, CSS, JavaScript och andra användarresurser. Det är också viktigt att notera att vissa webbsidor kan ha anti-skrapa åtgärder som blockerar automatisk nedladdning av sidor. Det är därför viktigt att kontrollera de lagar och regler som gäller för den specifika webbsidan och se till att du har rättigheter att ladda ner dess innehåll.

## Se även

- [Java URL-klassens officiella dokumentation](https://docs.oracle.com/en/java/javase/11/docs/api/java.base/java/net/URL.html)
- [Apache HTTP-klient för att ladda ner webbsidor i Java](https://hc.apache.org/httpcomponents-client-ga/tutorial/html/fundamentals.html)