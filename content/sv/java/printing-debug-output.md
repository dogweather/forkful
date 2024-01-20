---
title:                "Skriva ut felsökningsresultat"
html_title:           "Fish Shell: Skriva ut felsökningsresultat"
simple_title:         "Skriva ut felsökningsresultat"
programming_language: "Java"
category:             "Java"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/java/printing-debug-output.md"
---

{{< edit_this_page >}}

---
## Vad & Varför?

Att skriva ut debug-utdata är mekanismen att visa programmerarens tempinformation när de behöver spåra och fixa kodfel. Det hjälper till att förstå hur programkoden fungerar och får sitt jobb att fungera effektivt.

## Så här gör du:

Här är ett exempel för att skriva ut debug-meddelanden i Java: 

```Java
public class DebugDemo {
    public static void main(String[] args) {
        int sum = 0;
        for (int i = 0; i < 5; i++) {
            sum += i;
            System.out.println("Debug: i = " + i + ", sum = " + sum);
        }
    }
}
```

När du kör detta program, är output:

```Java
Debug: i = 0, sum = 0
Debug: i = 1, sum = 1
Debug: i = 2, sum = 3
Debug: i = 3, sum = 6
Debug: i = 4, sum = 10
```
På detta sätt kan vi se var exakt i loopen saker förändrades.

## Djupdykning 

Historiskt sett har programmet ‘printf’-debbuging, där ‘printf’ är ett inbyggt bibliotek i många programmersspråk, varit en vanlig lösning. Detta är dock inte alltid den bästa lösningen, särskilt för större programkod där utmatningen kan bli för överväldigande.

Alternativ innefattar att använda debuggare, som också kan ge mer detaljerad information om tillståndet för din kod vid givna punkter. Dessa verktyg kan dock vara tunga och i många fall är en enkel utskrift av debugdata den bästa lösningen.

Implementationen av debug-utdata i Java är ganska enkel, med `System.out.println()`-metoden som används för att skriva ut meddelanden till konsolen.

## Se också

1. [Oracle Java Tutorial](https://docs.oracle.com/javase/tutorial/): Ursprungliga Java-tutorials som gör att du kan förstå programmeringsspråket djupt.

2. [Java Debugging with Eclipse Tutorial](https://www.vogella.com/tutorials/EclipseDebugging/article.html): Detaljerad handledning om hur du använder Eclipse-verktyget för att debugga dina Java-program.

3. [Introduction to Java debugging](https://stackify.com/java-debugging-tips/): Mer information om debugging i Java.

---