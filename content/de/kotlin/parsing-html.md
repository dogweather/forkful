---
title:                "HTML Parsen"
html_title:           "Kotlin: HTML Parsen"
simple_title:         "HTML Parsen"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/kotlin/parsing-html.md"
---

{{< edit_this_page >}}

## Was ist damit gemeint und Warum? 
HTML-Parsing ist der Prozess des Lesens und Analysierens von HTML-Code. Programmierer tun dies, um Daten von einer Webseite zu extrahieren, um sie in anderen Anwendungen zu nutzen oder um die Struktur und den Inhalt einer Webseite zu verstehen.

## So geht's: 
```Kotlin 
// Importiere die benötigten Bibliotheken 
import org.jsoup.Jsoup 
import org.jsoup.nodes.Document 
import org.jsoup.select.Elements 

// Erstelle eine Verbindung zur Webseite 
val url = "https://www.example.com" 
val doc = Jsoup.connect(url).get() 

// Finde alle Links auf der Webseite 
val links: Elements = doc.select("a[href]") 

// Gebe die gefundenen Links aus 
for (link in links) { 
    println(link.attr("href") + " " + link.text()) 
} 
```

Output: 
https://www.example.com/Home Startseite 
https://www.example.com/About Us Über uns 
https://www.example.com/Contact Kontakt 

## Tiefer eingetaucht: 
HTML-Parsing hat sich im Laufe der Jahre weiterentwickelt und ist in vielen verschiedenen Programmiersprachen verfügbar. Alternativen zu Jsoup sind beispielsweise BeautifulSoup für Python und Nokogiri für Ruby. Beim Parsen von HTML ist es wichtig zu beachten, dass der Code fehleranfällig sein kann, da er von verschiedenen Quellen erstellt wird. Es ist daher wichtig, robuste Parser wie Jsoup zu verwenden, die mit unvollständigem oder fehlerhaftem Code umgehen können.

## Siehe auch: 
- Offizielle Jsoup-Dokumentation: https://jsoup.org/
- BeautifulSoup: https://www.crummy.com/software/BeautifulSoup/
- Nokogiri: https://nokogiri.org/