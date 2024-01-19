---
title:                "Ladda ner en webbsida"
html_title:           "Bash: Ladda ner en webbsida"
simple_title:         "Ladda ner en webbsida"
programming_language: "Java"
category:             "Java"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/java/downloading-a-web-page.md"
---

{{< edit_this_page >}}

## Vad & Varför?

Nedladdning av en webbsida innebär att man hämtar HTML-koden för en särskild URL. Detta gör programmerare för att programmiskt analysera, manipulera eller spara informationen på sidan.

## Så här gör du:

För att ladda ner en webbsida i Java kan du använda `java.net.URL` och `java.util.Scanner` klasserna. Använd `URL` klassen för att öppna en anslutning till din önskade webbsida och `Scanner` för att läsa innehållet.

```Java
import java.net.URL;
import java.util.Scanner;

public class Main {
    public static void main(String[] args) throws Exception {
        URL url = new URL("http://www.example.com");
        Scanner scanner = new Scanner(url.openStream());
        
        while(scanner.hasNext()) {
            System.out.println(scanner.nextLine());
        }
        scanner.close();
    }
}
```
Koden använder `Scanner` för att läsa HTML-koden rad för rad, vilket resulterar i webbsidans fullständiga innehåll som skrivs ut på skärmen.

## Djupdykning

1. **Historisk kontext:** Nedladdning av webbsidor har varit en del av programmeringsspråket sedan dess uppkomst på 90-talet. Från början användes råa `Socket`-anslutningar för att ansluta till en webbserver och begära en webbsida.
2. **Alternativ:** Förutom `java.net.URL` kan du också använda bibliotek som Jsoup som gör det lätt att hantera och analysera HTML. 
3. **Detaljer:** `java.net.URL` öppnar en ström till webbsidan, och `Scanner` läser innehållet i den strömmen. Kom ihåg att alltid stänga `Scanner` när du är klar för att frigöra resurser.

## Se också:

1. [Oracle Java-docs: URL](https://docs.oracle.com/javase/9/docs/api/java/net/URL.html)
2. [Oracle Java-docs: Scanner](https://docs.oracle.com/javase/9/docs/api/java/util/Scanner.html)
3. [Jsoup: Java HTML-parser](https://jsoup.org/)