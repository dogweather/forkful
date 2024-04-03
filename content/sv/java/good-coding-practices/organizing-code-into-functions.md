---
date: 2024-01-26 01:10:25.068697-07:00
description: "Hur man g\xF6r: H\xE4r \xE4r ett klassiskt exempel \u2013 en funktion\
  \ f\xF6r att ber\xE4kna fakulteten av ett tal."
lastmod: '2024-03-13T22:44:37.792761-06:00'
model: gpt-4-1106-preview
summary: "H\xE4r \xE4r ett klassiskt exempel \u2013 en funktion f\xF6r att ber\xE4\
  kna fakulteten av ett tal."
title: Att organisera kod i funktioner
weight: 18
---

## Hur man gör:
Här är ett klassiskt exempel – en funktion för att beräkna fakulteten av ett tal.

```java
public class MathUtils {

    public static void main(String[] args) {
        int number = 5;
        int result = factorial(number);
        System.out.println("Fakultet av " + number + " är: " + result);
    }
    
    public static int factorial(int n) {
        if (n <= 1) {
            return 1;
        }
        return n * factorial(n - 1);
    }
}
```

Utmatningen skulle bli:
```
Fakultet av 5 är: 120
```

## Fördjupning
Innan funktioner blev en sak var kod trängd i monolitiska block, vilket gjorde felsökning likt att hitta en nål i en höstack. Numera hjälper inkapsling av funktionalitet i funktioner till att snabbt isolera problem. Alternativ inkluderar lambda-uttryck i Java eller metoder i objektorienterad programmering, som båda tjänar liknande syften. När du skriver en funktion, kom ihåg att: (1) Varje funktion bör ha ansvar för en enda uppgift och (2) funktionens namn bör tydligt beskriva dess syfte.

## Se även
För mer om att organisera kod:
- Clean Code av Robert C. Martin
- Refactoring: Improving the Design of Existing Code av Martin Fowler
- [Oracle Java-dokumentation om att definiera metoder](https://docs.oracle.com/javase/tutorial/java/javaOO/methods.html)
