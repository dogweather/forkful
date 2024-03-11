---
changelog:
- 2024-01-30, gpt-4-0125-preview, translated from English
date: 2024-01-30 19:11:44.863192-07:00
description: "I Java lar assosiative tabeller, eller ordb\xF8ker, deg lagre n\xF8\
  kkel-verdi-par for effektiv oppslag og manipulering av data. Programmerere bruker\
  \ dem til\u2026"
lastmod: '2024-03-11T00:14:14.199658-06:00'
model: gpt-4-0125-preview
summary: "I Java lar assosiative tabeller, eller ordb\xF8ker, deg lagre n\xF8kkel-verdi-par\
  \ for effektiv oppslag og manipulering av data. Programmerere bruker dem til\u2026"
title: Bruke associative tabeller
---

{{< edit_this_page >}}

## Hva og hvorfor?

I Java lar assosiative tabeller, eller ordbøker, deg lagre nøkkel-verdi-par for effektiv oppslag og manipulering av data. Programmerere bruker dem til oppgaver som å telle forekomster av elementer eller å knytte brukere til deres tillatelser fordi de tilbyr rask tilgang og oppdateringer.

## Hvordan:

Java har ikke innebygde assosiative tabeller som noen språk har, men det gir `Map`-grensesnittet og klasser som `HashMap` og `TreeMap` for å fylle den rollen. Slik bruker du en `HashMap`:

```Java
import java.util.HashMap;
import java.util.Map;

public class LearnMaps {
    public static void main(String[] args) {
        // Oppretter en HashMap
        Map<String, Integer> ageOfFriends = new HashMap<>();
        
        // Legger til elementer
        ageOfFriends.put("Alice", 24);
        ageOfFriends.put("Bob", 30);
        ageOfFriends.put("Charlie", 28);

        // Får tilgang til elementer
        System.out.println("Alice's alder: " + ageOfFriends.get("Alice"));
        
        // Håndtering av ikke-eksisterende nøkler
        System.out.println("Alder på noen som ikke er i kartet: " + ageOfFriends.getOrDefault("Dan", -1));

        // Itererer over elementer
        for (Map.Entry<String, Integer> entry : ageOfFriends.entrySet()) {
            System.out.println(entry.getKey() + " er " + entry.getValue() + " år gammel.");
        }
    }
}
```

Eksempel på utskrift:

```
Alice's alder: 24
Alder på noen som ikke er i kartet: -1
Alice er 24 år gammel.
Bob er 30 år gammel.
Charlie er 28 år gammel.
```

`HashMap` er bare én implementasjon. Hvis nøklene dine er unike og du trenger dem sortert, vurder `TreeMap`. For et kart som beholder rekkefølgen ved innsetting, er `LinkedHashMap` din venn.

## Dypdykking

Ordbøker i Java er en del av Collections Framework, introdusert i JDK 1.2, men har sett betydelige forbedringer over årene, inkludert introduksjonen av `forEach`-metoden i Java 8 for lettere iterasjon over oppføringer. Valget av kartimplementasjon (`HashMap`, `LinkedHashMap`, `TreeMap`) bør dikteres av dine spesifikke behov når det gjelder ordning og ytelse. For eksempel tilbyr `HashMap` O(1) tidsytelse for de grunnleggende operasjonene (get og put), forutsatt at hashfunksjonen sprer elementene skikkelig blant bøttene. Imidlertid, hvis du trenger sortering basert på naturlig ordning eller egendefinerte sammenligninger, er `TreeMap` veien å gå, som gir O(log n) tid for innsetting og oppslag.

Før `Map` ble introdusert, ble assosiative tabeller vanligvis implementert med to parallelle tabeller (en for nøkler, en for verdier) eller egendefinerte datastrukturer med mindre effektivitet. Nåværende alternativer til `Map` og dens implementasjoner kunne inkludere tredjepartsbiblioteker som tilbyr spesialiserte kart, som toveiskart (BiMap i Googles Guava-bibliotek) for tilfeller der du trenger å finne en nøkkel ved dens verdi effektivt. Imidlertid, for de fleste bruksområder i Java, er standardbibliotekets kart robuste og fleksible nok til å håndtere oppgaven.
