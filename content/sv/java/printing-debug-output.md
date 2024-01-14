---
title:    "Java: Utskrift av felsökningsutdata"
keywords: ["Java"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/sv/java/printing-debug-output.md"
---

{{< edit_this_page >}}

## Varför

Att skriva ut debug-output är ett viktigt verktyg för utvecklare. Det ger en möjlighet att enkelt identifiera och lösa eventuella buggar eller fel i koden.

## Hur man gör

För att skriva ut debug-output i Java kan du använda metoden ```System.out.println()```. Detta kommer att skriva ut en sträng till konsolen, vilket kan vara användbart för att visa värden på variabler eller för att markera vissa steg i koden.

Här är ett enkelt exempel:

```Java
int age = 25;
double weight = 65.5;
System.out.println("Min ålder är " + age + " år och min vikt är " + weight + " kg.");
```

Detta kommer att skriva ut följande i konsolen:

```Min ålder är 25 år och min vikt är 65.5 kg.```

Du kan också använda metoden ```System.out.format()``` för att formatera strängen innan den skrivs ut. Detta kan vara användbart när du vill visa värden med ett specifikt format, som till exempel med decimaler eller som en valuta.

Här är ett exempel på hur du kan använda ```System.out.format()```:

```Java
double price = 59.99;
System.out.format("Priset för produkten är %.2f kr.", price);
```

Detta kommer att skriva ut följande:

```Priset för produkten är 59.99 kr.```

## Djupdykning

När du använder debug-output är det viktigt att du bara skriver ut den information som faktiskt är relevant för att lösa problemet eller förstå koden. Att skriva ut för mycket information kan göra det svårt att hitta den information du letar efter och kan även påverka prestandan för din kod.

En annan användbar funktion för debug-output är att skriva ut felmeddelanden. Genom att använda ```System.err.println()``` istället för ```System.out.println()``` kan du skapa ett tydligt felmeddelande som hjälper dig att hitta och åtgärda fel i din kod.

## Se även

- [Debugging in Java: A Step by Step Guide](https://www.baeldung.com/java-debugging)
- [How to Write and Use Java Debug Logs](https://www.baeldung.com/java-debug-logging)
- [Debugging Best Practices](https://medium.com/@gustavonalle/debugging-best-practices-422b72a17d05)