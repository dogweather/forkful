---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:12:27.561168-07:00
description: "Att tolka HTML inneb\xE4r att gr\xE4va igenom m\xE4rkspr\xE5ket f\xF6\
  r att extrahera data som text, l\xE4nkar eller andra element. Vi g\xF6r det f\xF6\
  r att interagera med eller\u2026"
lastmod: '2024-02-25T18:49:36.080357-07:00'
model: gpt-4-0125-preview
summary: "Att tolka HTML inneb\xE4r att gr\xE4va igenom m\xE4rkspr\xE5ket f\xF6r att\
  \ extrahera data som text, l\xE4nkar eller andra element. Vi g\xF6r det f\xF6r att\
  \ interagera med eller\u2026"
title: Tolka HTML
---

{{< edit_this_page >}}

## Vad & Varför?

Att tolka HTML innebär att gräva igenom märkspråket för att extrahera data som text, länkar eller andra element. Vi gör det för att interagera med eller skrapa webbinnehåll, automatisera surfuppgifter eller testa webbapplikationer.

## Hur man gör:

Låt oss använda Jsoup, ett praktiskt bibliotek för att arbeta med verklig HTML. Först, lägg till beroendet:

```xml
<dependency>
    <groupId>org.jsoup</groupId>
    <artifactId>jsoup</artifactId>
    <version>1.15.2</version>
</dependency>
```

Nu till den roliga delen. Så här hämtar du en webbsidas titel och skriver ut den:

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

Utskrift:

```
Titel: Exempeldomän
```

Hur är det med att extrahera alla länkar?

```java
import org.jsoup.nodes.Element;
import org.jsoup.select.Elements;

// ... inuti main eller en annan metod
Elements links = doc.select("a[href]");
for (Element link : links) {
    System.out.println("Länk: " + link.attr("href"));
}
```

## Djupdykning

En gång i tiden blev HTML tämjt av regex-mönster, en metod både felbenägen och mardrömslik för komplexa dokument. Sen kom Jsoup i slutet av 00-talet, som erbjuder ett jQuery-liknande gränssnitt för Java för att tolka, traversera och manipulera HTML.

Jsoup är inte det enda valet. Det finns HtmlUnit för fullfjädrad webbapplikationstestning med JavaScript-stöd, men det är tyngre och mer komplicerat. För lättviktsuppgifter är Apache Commons Validator bra bara för att extrahera URL:er.

Under huven använder Jsoup en DOM-parser, som modellerar hela dokumentet i minnet som ett träd. Detta tillvägagångssätt gör det enkelt att välja och navigera HTML-strukturen. Dessutom är det förlåtande med slarvig HTML, åtgärdar problem på språng för att säkerställa robust tolkning.

Kom ihåg, när du skrapar, alltid kontrollera en webbplats `robots.txt` och användarvillkor för att undvika juridiska problem eller att bli IP-bannad.

## Se även

- Jsoup officiell dokumentation: https://jsoup.org/
- HtmlUnit: http://htmlunit.sourceforge.net/
- Apache Commons Validator: https://commons.apache.org/proper/commons-validator/
