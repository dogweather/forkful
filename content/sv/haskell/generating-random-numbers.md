---
title:                "Generera slumpmässiga nummer"
html_title:           "Haskell: Generera slumpmässiga nummer"
simple_title:         "Generera slumpmässiga nummer"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/haskell/generating-random-numbers.md"
---

{{< edit_this_page >}}

## Varför
Att generera slumpmässiga nummer är ett vanligt verktyg inom programmering och kan användas för allt från spel och simulationsprogram till kryptering och testning av kod. Genom att lära sig hur man genererar slumpmässiga nummer i Haskell, kan du öka din förståelse för språkets funktioner och skapa mer dynamiska och användbara program.

## Så här gör du
För att generera slumpmässiga nummer i Haskell använder vi funktionen ```randomR``` från standardbiblioteket "System.Random". Denna funktion tar emot en intervall av värden och returnerar ett slumpmässigt nummer inom detta intervall. Låt oss titta på ett exempel:

```Haskell
import System.Random

main = do
  num <- randomR (1, 10) --skapar ett slumpmässigt nummer mellan 1 och 10
  putStrLn ("Ditt slumpmässiga nummer är: " ++ show num)
```

### Resultat

```
Ditt slumpmässiga nummer är: 7
```

Genom att använda funktionen ```randomR``` kan vi också generera slumpmässiga tecken och strängar. Låt oss se hur det skulle se ut i kod:

```Haskell
import System.Random

main = do
  char <- randomR ('a', 'z') --skapar ett slumpmässigt tecken mellan a och z
  putStrLn ("Ditt slumpmässiga tecken är: " ++ [char])
  
  str <- sequence $ take 10 $ repeat (randomR ('a','z')) --skapar en slumpmässig sträng med 10 tecken
  putStrLn ("Din slumpmässiga sträng är: " ++ str)
```

### Resultat

```
Ditt slumpmässiga tecken är: k
Din slumpmässiga sträng är: divingngek
```

## Djupdykning
Vid generering av slumpmässiga nummer använder Haskell en algoritm kallad "Mersenne Twister". Detta är en pseudoslumpgenerator som baseras på matematisk beräkning och inte på faktiska slumpmässiga händelser. Detta innebär att generatorn kan återskapa samma sekvens av slumpmässiga nummer om den ges samma startvärde, vilket kan vara användbart för testning av kod. Det finns också andra funktioner och metoder för att generera slumpmässiga nummer i Haskell, som exempelvis "random", "randomRs" och "getStdGen". Genom att utforska och experimentera med dessa funktioner kan du skapa mer avancerade och mångsidiga program.

## Se även
- [Haskell.org](https://www.haskell.org/)
- [Haskell Standardbibliotek](https://hackage.haskell.org/package/base-4.16.0.0/docs/System-Random.html)
- [Mersenne Twister](https://en.wikipedia.org/wiki/Mersenne_Twister)