---
date: 2024-01-20 15:32:52.025900-07:00
description: "Hvordan: Parsing av HTML startet s\xE5 snart webapplikasjoner trengte\
  \ \xE5 interagere med websider utover \xE5 bare vise dem. Historisk har programmerere\
  \ brukt\u2026"
lastmod: '2024-04-05T21:53:41.645003-06:00'
model: unknown
summary: "Parsing av HTML startet s\xE5 snart webapplikasjoner trengte \xE5 interagere\
  \ med websider utover \xE5 bare vise dem."
title: Analyse av HTML
weight: 43
---

## Hvordan:
```Java
import org.jsoup.Jsoup;
import org.jsoup.nodes.Document;
import org.jsoup.nodes.Element;
import org.jsoup.select.Elements;

public class HtmlParser {
    public static void main(String[] args) {
        String html = "<html><head><title>Eksempel</title></head>"
                    + "<body><p>Dette er en <a href='http://eksempel.com'>lenke</a>.</p></body></html>";
        Document doc = Jsoup.parse(html);
        Element link = doc.select("a").first();
        
        System.out.println("Tekst: " + link.text());
        System.out.println("URL: " + link.attr("href"));
    }
}

/* Output:
Tekst: lenke
URL: http://eksempel.com
*/
```

## Dypdykk
Parsing av HTML startet så snart webapplikasjoner trengte å interagere med websider utover å bare vise dem. Historisk har programmerere brukt regex eller DOM-baserte metoder, men disse tilnærmingene har utfordringer, som kompleksitet eller rigiditet. 

Biblioteker som Jsoup har forenklet parsing ved å tilby en robust, fleksibel og brukervennlig metode. Jsoup, for eksempel, bruker en CSS-lignende selektor-syntaks som lar utviklere hente elementer effektivt og intuitivt. Alternativer inkluderer biblioteker som htmlparser og Apache's HTMLUnit.

Implementeringsdetaljer varierer, men den grunnleggende prosessen innebærer å laste HTML som en streng, bruke en parser for å bygge et dokumentobjektmodell (DOM)-tre, og så navigere og manipulere dette treet for å oppnå ønskede resultater.

## Se Også
- Jsoup hjemmeside: [Jsoup.org](https://jsoup.org/)
- W3C HTML spec: [w3.org/TR/html52/](https://www.w3.org/TR/html52/)
- Open Web Application Security Project (OWASP) Parsing HTML guide: [OWASP Validation Regex Repository](https://owasp.org/www-community/OWASP_Validation_Regex_Repository)
