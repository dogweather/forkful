---
title:                "Skriva till standardfel"
html_title:           "Kotlin: Skriva till standardfel"
simple_title:         "Skriva till standardfel"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/kotlin/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## Varför

Det finns olika anledningar till varför man ibland behöver använda sig av standard error-utskrift i Kotlin. Det kan vara för att felsöka, skriva ut vissa meddelanden eller bara för att få feedback under körning. Oavsett anledning är det en användbar funktion att ha tillgång till.

## Så här gör du

Att skriva till standard error i Kotlin är enkelt och kräver bara en liten förändring i din kod. Det finns två sätt att göra det på, antingen genom att använda "System.err" eller genom att använda "printStackTrace()".

Båda metoderna kan användas genom att placera dem inuti en "try-catch" block, som fångar ett visst fel och skriver ut det till standard error. Här är ett exempel på hur det skulle kunna se ut:

```Kotlin
try {
    // Kod som kan orsaka ett fel
} catch(exception: Exception) {
    System.err.println("Ett fel inträffade: ${exception.message}")
    exception.printStackTrace()
}
```

Det finns också ett annat sätt att skriva till standard error, vilket är att använda "printStackTrace()" direkt på ett exception, som visas nedan:

```Kotlin
try {
    // Kod som kan orsaka ett fel
} catch(exception: Exception) {
    exception.printStackTrace(System.err)
}
```

Båda metoderna ger samma resultat, där felet skrivs ut till standard error. Det är viktigt att notera att "printStackTrace()" också kan användas utanför en "try-catch" block för att skriva ut stack-trace-informstion.

## Djupdykning

Standard error är den ström som används för att skriva ut felmeddelanden när en applikation kör. Det är en av de viktigaste skälen till att man bör lära sig hur man skriver till den i Kotlin. Standard error är också en del av Java API och är därför tillgänglig för användning i Kotlin.

En annan viktig anledning till att använda standard error är för att få information om vilka delar av koden som orsakade ett exception eller fel. Detta är särskilt användbart vid utveckling och felsökning av en applikation.

Slutligen är det viktigt att notera att meddelanden som skrivs ut med "System.err" kommer att visas i rött i de flesta utvecklingsmiljöer, vilket gör det lättare att se dem bland andra utskrifter.

## Se även

- [The official Kotlin documentation on standard error](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin.system/-kotlin.-system.err/index.html)
- [A tutorial on error handling in Kotlin](https://kotlinlang.org/docs/reference/exceptions.html)