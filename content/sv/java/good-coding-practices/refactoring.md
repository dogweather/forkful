---
title:                "Refaktorisering"
aliases: - /sv/java/refactoring.md
date:                  2024-01-26T01:18:56.548129-07:00
model:                 gpt-4-0125-preview
simple_title:         "Refaktorisering"

tag:                  "Good Coding Practices"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/java/refactoring.md"
---

{{< edit_this_page >}}

## Vad & Varför?
Refaktorisering är processen att omstrukturera befintlig datorkod—ändra faktoriseringen—utan att ändra dess externa beteende. Programmerare gör detta för att förbättra den icke-funktionella attributen hos mjukvaran, förbättra läsbarheten, minska komplexiteten och göra koden mer underhållbar för framtida projekt.

## Hur man gör:
Låt oss ta en enkel Java-klass som skriker efter refaktorisering på grund av dess dåliga organisation och brist på klarhet.

```java
public class Calculator {
    public int calc(int op1, int op2, String operation) {
        if (operation.equals("add")) {
            return op1 + op2;
        } else if (operation.equals("subtract")) {
            return op1 - op2;
        } // Andra operationer...
    }
}
```

Efter refaktorisering har vi:

```java
public class Calculator {
    public int add(int operand1, int operand2) {
        return operand1 + operand2;
    }

    public int subtract(int operand1, int operand2) {
        return operand1 - operand2;
    }

    // Andra operationer...
}
```

Genom refaktorisering har vi förbättrat metodnamnen och parametrarna för läsbarhet och tagit bort behovet av ett villkorsgren inom en enda metod. Varje operation anger nu tydligt sitt syfte.

## Djupdykning:
Refaktorisering har sina rötter i Smalltalk-gemenskapen, med dess betoning på kodläsbarhet och objektorienterad design, men det tog verkligen fart i Java-världen i slutet av 90-talet och början av 00-talet, särskilt efter utgivningen av Martin Fowlers banbrytande bok, "Refaktorisering: Förbättring av designen av befintlig kod".

Det finns alternativ till refaktorisering, som att skriva om koden från grunden. Dock är refaktorisering ofta att föredra eftersom det involverar inkrementella förändringar som inte stör funktionaliteten i applikationen.

Implementeringsdetaljer när man refaktorerar i Java (eller något programmeringsspråk) kretsar kring att förstå kodlukter—indikatorer på djupare problem i koden. Några lukter inkluderar långa metoder, stora klasser, duplicerad kod och överdriven användning av primitiva typer. Genom att tillämpa refaktoriseringsmönster som Extrahera metod, Flytta metod eller Ersätt Temp med Fråga, kan utvecklare systematiskt ta itu med dessa lukter samtidigt som de säkerställer att koden förblir funktionell vid alla tidpunkter.

Automatiserade verktyg, som IntelliJ IDEA:s stöd för refaktorisering eller plugins för Eclipse, kan underlätta processen genom att automatisera refaktoreringar såsom att byta namn på variabler, metoder och klasser, extrahera metoder eller variabler, och flytta metoder eller klasser till olika paket eller namnrymder.

## Se även:
- Martin Fowlers "Refaktorisering: Förbättring av designen av befintlig kod": https://martinfowler.com/books/refactoring.html
- Refaktoreringstekniker på Refactoring.Guru: https://refactoring.guru/refactoring/techniques
- Automatiserad refaktorisering i Eclipse: https://www.eclipse.org/eclipse/news/4.18/jdt.php
- IntelliJ IDEA:s refaktoriseringsfunktioner: https://www.jetbrains.com/idea/features/refactoring.html

Var och en av dessa resurser ger antingen en grund för att förstå principerna för refaktorisering eller verktyg som kan utnyttjas för att sätta dessa principer i praktiken.
