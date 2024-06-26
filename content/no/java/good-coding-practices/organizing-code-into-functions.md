---
date: 2024-01-26 01:10:54.086677-07:00
description: "Hvordan: Her er et klassisk eksempel \u2013 en funksjon for \xE5 beregne\
  \ fakultet av et tall."
lastmod: '2024-03-13T22:44:40.672126-06:00'
model: gpt-4-1106-preview
summary: "Her er et klassisk eksempel \u2013 en funksjon for \xE5 beregne fakultet\
  \ av et tall."
title: Organisering av kode i funksjoner
weight: 18
---

## Hvordan:
Her er et klassisk eksempel – en funksjon for å beregne fakultet av et tall.

```java
public class MathUtils {

    public static void main(String[] args) {
        int number = 5;
        int result = factorial(number);
        System.out.println("Fakultetet av " + number + " er: " + result);
    }
    
    public static int factorial(int n) {
        if (n <= 1) {
            return 1;
        }
        return n * factorial(n - 1);
    }
}
```

Utdata vil være:
```
Fakultetet av 5 er: 120
```

## Dypdykk
Før funksjoner var en ting, var kode stappet inn i monolittiske blokker, og det å feilsøke var som å finne en nål i en høystakk. Nå hjelper innkapsling av funksjonalitet i funksjoner med å raskt isolere problemer. Alternativer inkluderer lambda uttrykk i Java eller metoder i objektorientert programmering, som begge tjener lignende formål. Når du skriver en funksjon, husk: (1) Hver funksjon bør ha et enkelt ansvar og (2) navnet på funksjonen bør tydelig beskrive dens formål.

## Se også
For mer om å organisere kode:
- Clean Code av Robert C. Martin
- Refactoring: Improving the Design of Existing Code av Martin Fowler
- [Oracle Java dokumenter om å definere metoder](https://docs.oracle.com/javase/tutorial/java/javaOO/methods.html)
