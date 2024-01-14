---
title:    "Haskell: Generering av slumpmässiga tal"
keywords: ["Haskell"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/sv/haskell/generating-random-numbers.md"
---

{{< edit_this_page >}}

# Varför
Att generera slumpmässiga tal är ett viktigt verktyg inom datavetenskap och kan användas för många olika ändamål, som att testa algoritmer och simulera verkliga scenarier.

# Hur man gör det
För att generera slumpmässiga tal i Haskell, använd funktionen `randomR` från standardbiblioteket `System.Random`. Nedan följer ett exempel på hur man kan generera fem slumpmässiga heltal mellan 1 och 10:

```Haskell
import System.Random

main = do
  gen <- getStdGen
  let randomNumbers = take 5 (randomRs (1, 10) gen) :: [Int]
  print randomNumbers
```

Detta kommer att producera en utmatning som liknar följande:

```
[4,8,2,9,3]
```

Vi börjar med att importera `System.Random` och sedan skapa en slumpgenerator `gen` genom att använda funktionen `getStdGen`. Därefter använder vi `randomRs` för att generera en lista av slumpmässiga tal i intervallet 1-10 och sedan begränsar vi den till de första fem talen med `take`. Slutligen skriver vi ut de slumpmässiga talen med `print`.

# Fördjupning
I Haskell finns det flera sätt att generera slumpmässiga tal, men `randomR` är den vanligaste och enklaste metoden. Det finns också flera olika typer av slumpgeneratorer, varav den vanligaste är en "pseudo-random generator" som använder en seed för att generera en följd av tal som kan uppfattas som slumpmässiga. Det är viktigt att notera att dessa tal inte är helt slumpmässiga, men för de flesta ändamål är de tillräckligt bra.

Om man vill generera andra typer av värden, som till exempel slumpmässiga bokstäver eller listor, kan man använda sig av funktionen `random` från samma bibliotek. Det finns också olika metoder för att skräddarsy slumpgeneratorer, som att ange en annan seed eller begränsa utmatningen till ett visst intervall.

# Se även
- [Haskell documentation: System.Random](https://hackage.haskell.org/package/random/docs/System-Random.html)
- [Haskell Wiki: Random streams](https://wiki.haskell.org/Random_streams)
- [A Gentle Introduction to Haskell: Randomness](https://www.haskell.org/tutorial/goodies.html#randomness)