---
date: 2024-01-26 03:50:05.803260-07:00
description: "La oss si at du har et enkelt Java-program som oppf\xF8rer seg merkelig,\
  \ og du kan ikke finne ut hvorfor. Her er hvordan du ville fyrt opp en debugger\
  \ ved \xE5\u2026"
lastmod: '2024-03-13T22:44:40.671198-06:00'
model: gpt-4-0125-preview
summary: "La oss si at du har et enkelt Java-program som oppf\xF8rer seg merkelig,\
  \ og du kan ikke finne ut hvorfor."
title: "\xC5 bruke en feils\xF8ker"
weight: 35
---

## Hvordan:
La oss si at du har et enkelt Java-program som oppfører seg merkelig, og du kan ikke finne ut hvorfor. Her er hvordan du ville fyrt opp en debugger ved å bruke Eclipse, en av de populære IDE-ene for Java-utvikling:

Først, sørg for at du har satt et brytepunkt. Deretter, høyreklikk på filen, velg 'Debugg som', og klikk på 'Java-applikasjon'.

```Java
public class DebuggEksempel {
    public static void main(String[] args) {
        int a = 5;
        int b = 0;
        // Sett et brytepunkt her
        int resultat = del(a, b);
        System.out.println("Resultatet er: " + resultat);
    }

    private static int del(int teller, int nevner) {
        // Et annet godt sted for et brytepunkt
        return teller / nevner;
    }
}
```

Ved å gjøre dette, vil programmet ditt pause ved brytepunktet, og du kan inspisere variabler, gå gjennom koden linje for linje, og se hvordan programmet ditt oppfører seg.

Eksempel på utskrift (i en debugger-konsoll):
```
Brytepunkt truffet ved linje: int resultat = del(a, b);
```

## Dypdykk
Konseptet med feilsøking har vært rundt siden de tidlige dagene av programmering. Legenden har det til at uttrykket "bug" faktisk kom fra en ekte møll funnet inne i en datamaskin av Grace Hopper, en pioner innen feltet. Hurtig frem til i dag, og vi har sofistikerte IDE-er som IntelliJ IDEA, Eclipse og NetBeans som pakker kraftige feilsøkere.

Alternativer til IDE-feilsøkere inkluderer logging, utskriftssetninger (fattigmanns debugger), påstander, og selvstendige feilsøkingsverktøy som jdb (Java Debugger) som er en del av Java Development Kit (JDK).

En debugger fungerer ved å tillate programmereren å pause utførelsen (brytepunkter), gå gjennom koden, inspisere variabelverdier, modifisere disse verdiene på stedet, og til og med kjøre kode blokk for blokk. Bruken av en debugger anses ofte som en uvurderlig teknikk for å utvikle komplekse applikasjoner der det å spore ned den eksakte kodelinjen som forårsaker et problem kan være som å finne en nål i en høystakk.

## Se også
- Den offisielle Oracle-dokumentasjonen om feilsøking: [Oracle Java SE Feilsøking](https://docs.oracle.com/javase/8/docs/technotes/tools/windows/jdb.html)
- VisualVM, et visuelt verktøy som integrerer flere kommandolinje JDK-verktøy og lette profileringsegenskaper: [VisualVM](https://visualvm.github.io/)
