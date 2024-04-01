---
date: 2024-01-26 03:50:11.716359-07:00
description: "Att anv\xE4nda en debugger inneb\xE4r att man anv\xE4nder ett verktyg\
  \ f\xF6r att testa och r\xE4tta till buggar i koden. Programmerare g\xF6r det f\xF6\
  r att f\xF6rst\xE5 fl\xF6det i\u2026"
lastmod: '2024-03-13T22:44:37.791853-06:00'
model: gpt-4-0125-preview
summary: "Att anv\xE4nda en debugger inneb\xE4r att man anv\xE4nder ett verktyg f\xF6\
  r att testa och r\xE4tta till buggar i koden. Programmerare g\xF6r det f\xF6r att\
  \ f\xF6rst\xE5 fl\xF6det i\u2026"
title: "Att anv\xE4nda en debugger"
---

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
