---
title:                "Att använda en debugger"
date:                  2024-01-26T03:50:11.716359-07:00
model:                 gpt-4-0125-preview
simple_title:         "Att använda en debugger"

tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/java/using-a-debugger.md"
---

{{< edit_this_page >}}

## Vad & Varför?
Att använda en debugger innebär att man använder ett verktyg för att testa och rätta till buggar i koden. Programmerare gör det för att förstå flödet i sina applikationer, identifiera felkällor och verifiera logik under körning.

## Hur man gör:
Låt säga att du har ett enkelt Java-program som strular, och du kan inte lista ut varför. Här är hur du skulle starta en debugger med Eclipse, en av de populära IDE:erna för Java-utveckling:

Först, se till att du har satt en brytpunkt. Sedan högerklickar du på filen, väljer 'Debugga som', och klickar på 'Java-program'.

```Java
public class DebugExample {
    public static void main(String[] args) {
        int a = 5;
        int b = 0;
        // Sätt en brytpunkt här
        int resultat = dela(a, b);
        System.out.println("Resultatet är: " + resultat);
    }

    private static int dela(int täljare, int nämnare) {
        // En annan bra plats för en brytpunkt
        return täljare / nämnare;
    }
}
```

Genom att göra detta kommer ditt program att pausa vid brytpunkten, och du kan inspektera variabler, stega igenom koden rad för rad, och observera hur ditt program beter sig.

Exempelutmatning (i en debuggerkonsol):
```
Brytpunkt träffad på rad: int resultat = dela(a, b);
```

## Fördjupning
Konceptet med att felsöka har funnits sedan programmeringens tidiga dagar. Legenden säger att termen "bugg" faktiskt kommer från en verklig insekt (mott) som hittades inuti en dator av Grace Hopper, en pionjär inom området. Fram till idag har vi sofistikerade IDE:er som IntelliJ IDEA, Eclipse och NetBeans som innehåller kraftfulla debuggers.

Alternativ till IDE-debuggers inkluderar loggning, utskriftsuttalanden (stackars mans debugger), påståenden, och fristående felsökningsverktyg som jdb (Java Debugger) som är en del av Java Development Kit (JDK).

En debugger fungerar genom att tillåta programmeraren att pausa exekveringen (brytpunkter), stega igenom koden, inspektera variabelvärden, ändra dessa värden på flygande fot, och även köra kodblock för kodblock. Användning av en debugger anses ofta vara en ovärderlig teknik för utveckling av komplexa applikationer där det att hitta den exakta raden kod som orsakar ett problem kan liknas vid att leta efter en nål i en höstack.

## Se även
- Den officiella Oracle-dokumentationen om debugging: [Oracle Java SE Debugging](https://docs.oracle.com/javase/8/docs/technotes/tools/windows/jdb.html)
- VisualVM, ett visuellt verktyg som integrerar flera kommandoradsverktyg från JDK och lättviktsprofilering: [VisualVM](https://visualvm.github.io/)