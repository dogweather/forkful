---
title:                "Analysera html"
html_title:           "Java: Analysera html"
simple_title:         "Analysera html"
programming_language: "Java"
category:             "Java"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/java/parsing-html.md"
---

{{< edit_this_page >}}

Vad & Varför?
När vi pratar om att "parsa HTML" så menar vi att konvertera HTML-kod till ett strukturerat format som en dator kan förstå och bearbeta. Detta är användbart för programmerare eftersom det ger dem möjlighet att automatisera processer som involverar HTML, till exempel att söka efter specifika element eller extrahera data från en webbsida.

Hur fungerar det?
I Java kan vi använda verktyget Jsoup för att parsa HTML. Detta verktyg gör om HTML-koden till ett trädliknande strukturerat format, vilket gör det lättare att navigera och hämta data från. Här är ett exempel på hur vi kan använda det för att hämta innehållet i en <h1> tagg:

```Java
Document doc = Jsoup.connect("https://www.example.com").get();
Element h1 = doc.selectFirst("h1");
System.out.println(h1.text());
```

I det här fallet så hämtar vi in hela HTML-koden från webbsidan example.com och använder sedan selectFirst-metoden för att välja den första <h1> taggen som vi hittar. Vi använder sedan metoden .text() för att få ut texten som finns mellan <h1> taggen.

Djupdykning:
Parsning av HTML har funnits sedan de tidiga dagarna av webben, när webbsidor först började skapas med HTML-kod. Innan verktyg som Jsoup fanns tillgängliga, var detta en mycket mer komplicerad process som ofta innebar att man behövde skriva mycket kod för att extrahera och bearbeta data från en webbsida.

Alternativ till Jsoup inkluderar verktyg som HTML Parser och TagSoup. Dessa har liknande funktioner som Jsoup, men de har olika fördelar och nackdelar beroende på vad man behöver göra med HTML-koden.

Se även:
- Officiell dokumentation för Jsoup: https://jsoup.org/
- HTML Parser: https://htmlparser.sourceforge.io/
- TagSoup: https://github.com/veithen/tagsoup