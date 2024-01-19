---
title:                "Utskrift av feilsøkingsresultat"
html_title:           "Arduino: Utskrift av feilsøkingsresultat"
simple_title:         "Utskrift av feilsøkingsresultat"
programming_language: "Java"
category:             "Java"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/java/printing-debug-output.md"
---

{{< edit_this_page >}}

## Hva & Hvorfor?
Printerfeilsøkingsoutput er en måte programmerere legger inn midlertidig kode for å spore feil eller betingelser gang imellom. Vi bruker det for å forstå hvorfor og hvordan noe uventet skjedde i koden.

## Hvordan gjøre det:
Bruk `System.out.println()` for å skrive ut til konsollen. Her er et eksempel i Java:

```Java
public class DebugExample {
    public static void main(String[] args) {
        int x = 10;
        int y = 0;
        
        System.out.println("Starting the division operation...");
        
        try {
            int z = x / y;
        } catch(Exception e) {
            System.out.println("Caught an error: " + e);
        }
        
        System.out.println("Ended the operation.");
    }
}
```
Output:

```
Starting the division operation...
Caught an error: java.lang.ArithmeticException: / by zero
Ended the operation.
```

Du får en umiddelbar innsikt i hva som skjer underveis i koden.

## Dypdykk
Å printe debug-output er praksis som går tilbake til de tidligste dagene av programmering. Alternativer inkluderer bruken av feilsøkingsverktøy som lar deg «steg-for-steg» gjennom koden og inspisere data. Men, når det er sagt, er å printe debug-output ofte raskere og mer intuitivt for enkle feilsøkinger.

Husk å fjerne alle `System.out.println()`-statements fra produksjonskode fordi de vil bremse ytelsen. Java bruker en synkronisert ut-datastrøm for `System.out.println()`, noe som kan føre til at tråder blokkeres mens de venter på at utgangene skal bli klare.

## Se Også
Feilsøking i Java: https://www.baeldung.com/java-debugging-strategies
Logger i Java: https://www.vogella.com/tutorials/Logging/article.html
IntelliJ IDEA debugging: https://www.jetbrains.com/help/idea/debugging-your-first-java-application.html