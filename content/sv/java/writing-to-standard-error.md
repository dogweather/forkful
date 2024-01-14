---
title:                "Java: Skriva till standardfel"
programming_language: "Java"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/java/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## Varför
Att skriva till standardfel är ett viktigt verktyg i Java-programmering, eftersom det ger möjlighet att felmeddelanden skrivna till standard output (eller console) att ignoreras eller försvinna bland annan utdata. Genom att skicka felmeddelanden till standardfel, säkerställer vi att de syns och kan åtgärdas av utvecklaren.

## Hur man gör det
För att skriva till standardfel i Java, behöver du använda objektet System.err och metoden println() som skriver ut en sträng till standardfel. Här är ett exempel: 

```Java
System.err.println("Detta är ett exempel på ett felmeddelande"); 
```

Om vi kör detta kodblock får vi följande utdata i standardfel:

```Java
Detta är ett exempel på ett felmeddelande
```

Det är viktigt att komma ihåg att använda System.err.println() bara när det är nödvändigt, eftersom det kan göra din kod svårare att läsa och förstå.

## Djupdykning
När vi skriver till standardfel, skickar vi egentligen utströmmen till felhanteringsmekanismen i Java. Detta gör att felmeddelanden kan åtgärdas på olika sätt beroende på miljön där koden körs. Till exempel, om vi kör koden i en webbapplikation, kan felmeddelandena skickas till en loggfil som utvecklaren kan kolla på senare.

Det är också värt att notera att standardfel inte bara används för felmeddelanden utan också för att skriva ut information om varningar eller avbrott i koden. Att använda System.err.println() ger utvecklare möjlighet att se dessa meddelanden och felsöka sin kod mer effektivt.

## Se också
- [Java API Dokumentation](https://docs.oracle.com/javase/8/docs/api/)
- [Java Felsökning: Att använda System.err.println()](https://www.baeldung.com/java-system-out-println-vs-system-err-println)
- [Javaskolan: System.err.println()](http://javaskolan.se/src/system.err.println.html)