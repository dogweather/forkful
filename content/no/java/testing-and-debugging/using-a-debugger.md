---
title:                "Å bruke en feilsøker"
date:                  2024-01-26T03:50:05.803260-07:00
model:                 gpt-4-0125-preview
simple_title:         "Å bruke en feilsøker"

tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/java/using-a-debugger.md"
---

{{< edit_this_page >}}

## Hva & Hvorfor?
Å bruke en debugger betyr å benytte et verktøy for å teste og fikse feil i koden din. Programmerere gjør dette for å forstå flyten i applikasjonene sine, identifisere kildene til feil, og verifisere logikken under utførelse.

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
