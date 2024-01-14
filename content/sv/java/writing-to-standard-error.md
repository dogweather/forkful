---
title:    "Java: Skriva till standardfel"
keywords: ["Java"]
---

{{< edit_this_page >}}

## Varför

Att skriva till standard error kan vara till stor hjälp när du felsöker din Java-kod. Genom att skicka felmeddelanden till standard error istället för standard output kan du hålla din kodren renare och lättare att läsa.

## Hur man gör det

Att skriva till standard error i Java är enkelt. Använd bara `System.err.println()` istället för `System.out.println()`. Det här kommer att skriva ut ditt meddelande till standard error istället för standard output.

```java
System.err.println("Det här är ett felmeddelande");
```

Medan standard output skrivs ut i konsolen medan du kör din kod, kommer standard error att skrivas ut till konsolen oavsett om det finns eventuella fel eller inte. Detta är särskilt användbart när du kör din kod i ett skript eller i en annan miljö där standard output inte syns.

## Djupdykning

Att skriva till standard error kan även hjälpa dig att identifiera och åtgärda fel i din kod. Istället för att bara skriva ut felmeddelanden, kan du också använda `System.err` för att skriva ut det faktiska felet som har inträffat.

```java
try {
    int result = 5 / 0; // det här kommer att kasta ett ArithmeticException
} catch (ArithmeticException e) {
    System.err.println("Inte möjligt att dela med noll");
    e.printStackTrace(System.err); // här skriver vi även ut felet till standard error
}
```

Det finns många andra användbara metoder som `System.err` har att erbjuda för att hantera fel i din kod. Det är värt att kolla in Java-dokumentationen för att lära dig mer om dessa.

## Se även

- [Java-Dokumentation för System.err](https://docs.oracle.com/javase/8/docs/api/java/lang/System.html#err)
- [Java Felsökningshandledning](https://www.oracle.com/technetwork/articles/java/fault-exceptions-139353.html)