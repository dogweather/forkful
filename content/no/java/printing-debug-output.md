---
title:                "Utskrift av feilsøkingsutdata"
html_title:           "Java: Utskrift av feilsøkingsutdata"
simple_title:         "Utskrift av feilsøkingsutdata"
programming_language: "Java"
category:             "Java"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/java/printing-debug-output.md"
---

{{< edit_this_page >}}

## Hva & Hvorfor?
Å printe ut feilsøkingsmeldinger er en viktig del av programmering. Det er en måte for utviklere å se hva som skjer i programmet under kjøring, og å finne og fikse eventuelle feil eller problemer. Det kan også hjelpe til med å forstå hvordan koden fungerer og optimalisere den for bedre ytelse.

## Slik gjør du:
For å printe ut debug output i Java, kan du bruke metoden `System.out.println()`. Denne metoden tar inn en parameter og skriver ut verdien til denne parameteren i konsollen. For eksempel:

```
Java
int num = 5;
System.out.println("Verdien til num er: " + num);

// Output: Verdien til num er: 5
```

Du kan også bruke `System.out.print()` for å printe uten å legge til en ny linje. Dette kan være nyttig hvis du vil printe en del av en setning og deretter skrive mer ut på samme linje. For eksempel:

```
Java
int num1 = 10;
int num2 = 20;

System.out.print("Summen av num1 og num2 er: ");
System.out.println(num1 + num2);

// Output: Summen av num1 og num2 er: 30
```

## Dypdykk:
Å printe ut feilsøkingsmeldinger har vært en del av programmering siden de tidlige dagene. Før i tiden var det vanlig å bruke såkalte "print statements", der man printet ut variabler og strings på ulike steder i koden for å se hva som skjer under kjøring. Dette kunne være en tidkrevende og tungvint prosess.

Et alternativ til å printe ut feilsøkingsmeldinger er å bruke et debugger-verktøy. Dette er et programmeringsverktøy som lar deg pause koden og visualisere hva som skjer i ulike deler av programmet. Debugger-verktøy kan være mer effektive i å finne og fikse feil, og er ofte mer avanserte og tilpassbare enn å printe ut meldinger.

Implementasjonen av debugging i Java er enkelt og intuitivt takket være `System.out`-metodene som er inkludert i språket. Det er også mulig å bruke andre metoder for å skrive ut feilsøkingsmeldinger, som f.eks. `Log`-klassen i `java.util.logging`-biblioteket.

## Se også:
[Java debugging tutorial](https://www.journaldev.com/13163/how-to-debug-java-application-in-eclipse)  
[Eclipse debugger tutorial](https://www.tutorialspoint.com/eclipse/eclipse_debugging_programs.htm)