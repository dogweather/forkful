---
title:                "Analysering av html"
html_title:           "Java: Analysering av html"
simple_title:         "Analysering av html"
programming_language: "Java"
category:             "Java"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/java/parsing-html.md"
---

{{< edit_this_page >}}

# Hva & Hvorfor?
Parsing av HTML er en prosess der en datamaskin leser og tolker kode skrevet i HTML-format. Dette er viktig for programmerere fordi det gjør det mulig for en datamaskin å forstå og behandle nettsider og deres innhold.

# Hvordan:
For å parse HTML i Java, kan du bruke biblioteker som "JSoup" eller "Jericho HTML Parser". Nedenfor er et enkelt eksempel på hvordan du kan bruke "JSoup" til å hente alle linker fra en nettside:

```Java
Document doc = Jsoup.connect("https://www.example.com").get(); // Hent nettsiden
Elements links = doc.select("a[href]"); // Velg alle linker
for (Element link : links) { // Gå gjennom alle linker
    System.out.println(link.attr("href")); // Print ut linken
}
```

Dette eksempelet vil skrive ut alle linker på nettsiden "https://www.example.com".

# Dykke dypere:
Parsing av HTML ble først utviklet på 1990-tallet som en del av utviklingen av World Wide Web. Før dette var det vanlig å bruke manuelle metoder for å behandle nettsteddata. Det finnes også andre programmeringsspråk som har innebygde funksjoner for å parse HTML, som for eksempel Python og PHP.

Når du parser HTML i Java, er det viktig å huske å håndtere eventuelle feil som kan oppstå, som for eksempel ugyldig HTML-kode. Du bør også være bevisst på hvor mange ressurser (CPU og minne) du bruker, spesielt hvis du parser store mengder data.

# Se også:
- [JSoup](https://jsoup.org/)
- [Jericho HTML Parser](http://jericho.htmlparser.net/docs/index.html)