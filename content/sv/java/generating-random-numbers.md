---
title:                "Java: Generera slumpmässiga nummer"
simple_title:         "Generera slumpmässiga nummer"
programming_language: "Java"
category:             "Java"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/java/generating-random-numbers.md"
---

{{< edit_this_page >}}

## Varför

Att generera slumpmässiga siffror kan vara en användbar funktion inom många olika områden inom programmering. Det kan hjälpa till att skapa variation och slumpmässighet i en applikation, vilket kan vara användbart vid testning och simuleringar.

## Hur man gör det

Det finns flera olika sätt att generera slumpmässiga siffror i Java, men en vanlig metod är att använda klassen Random. Nedanför finns ett exempel på hur man kan använda denna klass för att generera en slumpmässig siffra mellan 1 och 100.

```Java
Random rand = new Random();
int randomNumber = rand.nextInt(100) + 1;
System.out.println("Slumpmässig siffra: " + randomNumber);
```

**Output:**

`Slumpmässig siffra: 64`

Detta kodblock skapar en instans av klassen Random och använder sedan metoden nextInt() för att generera en slumpmässig siffra. Talet som specificeras i parentesen anger den högsta siffran som kan genereras, och i detta fall är det 100. Genom att lägga till 1 kommer talet att vara mellan 1 och 100 istället för mellan 0 och 99.

Det finns också andra metoder inuti Random-klassen som kan användas för att generera slumpmässiga siffror av olika datatyper, såsom long och double. Det finns också andra klasser som kan användas för att generera slumpmässiga tal på olika sätt, såsom ThreadLocalRandom och SecureRandom.

## Djupdykning

En vanlig fråga när det gäller generering av slumpmässiga siffror är om dessa siffror verkligen är slumpmässiga. Svaret på det är att de inte är 100% slumpmässiga, men de är tillräckligt slumpmässiga för att vara användbara inom programutveckling.

Många av de klasser och metoder som används för att generera slumpmässiga siffror använder sig av en algoritm som baserar sig på en så kallad "seed". Denna seed är en startpunkt för algoritmen och om samma seed används kommer algoritmen alltid att generera samma siffror i samma ordning. Med andra ord, siffrorna är inte verkligt slumpmässiga utan är baserade på en förutbestämd ordning.

En annan aspekt att tänka på när det gäller slumpmässiga siffror är vikten av en hög upplösning på systemklockan. Om klockan har låg upplösning kan det påverka slumpmässigheten av de genererade siffrorna.

## Se också

- [Java Random-klassen](https://docs.oracle.com/javase/8/docs/api/java/util/Random.html)
- [Java ThreadLocalRandom-klassen](https://docs.oracle.com/javase/8/docs/api/java/util/concurrent/ThreadLocalRandom.html)
- [Java SecureRandom-klassen](https://docs.oracle.com/javase/8/docs/api/java/security/SecureRandom.html)