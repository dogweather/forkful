---
title:                "Ladda ner en webbsida"
html_title:           "Kotlin: Ladda ner en webbsida"
simple_title:         "Ladda ner en webbsida"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/kotlin/downloading-a-web-page.md"
---

{{< edit_this_page >}}

Vad & Varför?

Att ladda ner en webbsida innebär att hämta innehållet från en viss webbsida och spara det lokalt på din dator. Detta kan vara till nytta för programmerare eftersom det ger dem möjlighet att arbeta med webbinnehåll utan att behöva vara online.

Hur to:

Exempel 1: Ladda ner och skriv ut innehållet från en webbsida.

```Kotlin
val url = "www.example.com"
val content = URL(url).readText()
println(content)
```

Output: Innehållet från webbsidan på www.example.com kommer att skrivas ut i konsolen.

Exempel 2: Spara innehållet från en webbsida till en fil.

```Kotlin
val url = "www.example.com"
val content = URL(url).readText()
File("myFile.txt").writeText(content)
```

Output: Innehållet från webbsidan på www.example.com kommer att sparas i filen "myFile.txt".

Djupdykning:

Historisk kontext: Att ladda ner webbsidor har funnits sedan början av webben. Det var en vanlig funktion i webbläsare på 90-talet för att kunna använda webben offline. Idag används det främst av programmerare för utveckling och testning.

Alternativ: Det finns flera alternativ för att ladda ner webbsidor, men Kotlin's standard library som vi visade i exemplet ovan är ett enkelt och effektivt sätt att åstadkomma detta. Det finns också olika tredjepartsbibliotek som kan användas för mer avancerade funktioner.

Implementeringsdetaljer: I exemplet används funktionen "readText()" från klassen URL som finns i Kotlin's standard library för att läsa innehållet från webbsidan som en sträng. Det är också möjligt att använda "readLines()" för att läsa innehållet som en lista av rader eller "openStream()" för att läsa innehållet från webbsidan som en InputStream.

Se också:

- Kotlin's standard library: https://kotlinlang.org/api/latest/jvm/stdlib/kotlin.text/
- Tredjepartsbibliotek för att ladda ner webbsidor: https://www.baeldung.com/java-download-file
- Dokumentation för Java's URL-klass som också kan användas i Kotlin: https://docs.oracle.com/javase/8/docs/api/java/net/URL.html