---
title:                "Analys av html"
html_title:           "C: Analys av html"
simple_title:         "Analys av html"
programming_language: "C"
category:             "C"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/c/parsing-html.md"
---

{{< edit_this_page >}}

# Vad & Varför?
HTML-analys, eller parsing, är en process där en dator tar en HTML-kod och bryter ner den i olika delar för att förstå dess struktur och innehåll. Detta är viktigt för programmerare eftersom det gör det möjligt för dem att manipulera eller extrahera data från HTML-dokument.

# Hur gör man:
```C
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

// Enkelt exempel på att analysera en HTML-tagg
int main(void) {
    char html_data[] = "<h1>Welcome to my website</h1>";
    char tag_name[50];
    char tag_content[100];

    // Skildra taggnamnet från innehållet
    sscanf(html_data, "<%[^>]>%[^<]", tag_name, tag_content);

    // Skriv ut taggnamnet och innehållet
    printf("Taggnamn: %s\n", tag_name);
    printf("Innehåll: %s\n", tag_content);

    return 0;
}
```
```C
// Resultat:
Taggnamn: h1
Innehåll: Welcome to my website
```

# Djupdykning:
HTML-analys är en fundamentell del av webbutveckling och har funnits sedan tidigt 1990-tal. Tidiga webbläsare som Netscape Navigator och Internet Explorer var ansvariga för att tolka och rendera HTML-dokument. Idag finns det alternativ till HTML-analys, såsom DOM-parser och XPath, men HTML-analys är fortfarande den vanligaste metoden för att manipulera och extrahera data från HTML.

Det finns också flera bibliotek tillgängliga för att underlätta HTML-analys i C, såsom libxml och glib. Dessa bibliotek ger funktioner som kan användas för att hantera HTML-dokument.

# Se även:
- [W3Schools HTML Tutorial](https://www.w3schools.com/html/html_intro.asp)
- [libxml](http://www.xmlsoft.org/html/index.html)
- [GLib](https://developer.gnome.org/glib/)