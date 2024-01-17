---
title:                "Skriva till standardfel"
html_title:           "Java: Skriva till standardfel"
simple_title:         "Skriva till standardfel"
programming_language: "Java"
category:             "Java"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/java/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## Vad & Varför?

Att skriva till standard error är en vanlig praxis bland programmerare för att rapportera fel eller avvikelser i ett program. Istället för att skriva ut felmeddelanden till standardutmatningen, som kan vara svår att hitta bland annan utmatning, kan man skriva dem till standard error för tydligare och mer strukturerad rapportering.

## Hur man gör:

```
Java System:
System.err.println("Detta är ett felmeddelande!"); 
```
Output: ```Detta är ett felmeddelande!```

```
Try-Catch:
try {
  // kodblock där felet kan uppstå
} catch (Exception e) {
  System.err.println("Detta är ett felmeddelande!");
}
```
Output: ```Detta är ett felmeddelande!```

## Djupdykning:

Att skriva till standard error är en standardiserad metod för att rapportera fel och varningar i program. Det kan göras genom att använda ```System.err.println()```, som utnyttjar standard error-flödet i JVM. Alternativet till att skriva till standard error är att använda loggfilning, men det är mer lämpligt för att spåra händelser i produktion istället för att rapportera fel under utveckling.

Det är också värt att nämna att flera programmeringsspråk, som till exempel C++, Java och Python, tillåter att man skriver till standard error på liknande sätt.

## Se också:

[Java System klassens dokumentation](https://docs.oracle.com/javase/8/docs/api/java/lang/System.html)