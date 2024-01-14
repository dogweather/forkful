---
title:    "Clojure: Generering av slumpmässiga nummer"
keywords: ["Clojure"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/sv/clojure/generating-random-numbers.md"
---

{{< edit_this_page >}}

# Varför

Att generera slumpmässiga tal är ett vanligt problem inom programmering och kan vara användbart för många olika ändamål, såsom spelutveckling, simuleringar och kryptografi.

# Hur man gör det

För att generera slumpmässiga tal i Clojure, kan använda funktionen `rand`, som returnerar ett tal mellan 0 och 1 (exklusive 1). Till exempel:

```Clojure
(rand)
```

Detta kommer att generera ett slumpmässigt tal varje gång koden körs. Om du vill generera ett heltal, kan du använda `rand-int` istället, som tar ett argument för det önskade intervallet. Till exempel:

```Clojure
(rand-int 10) ; genererar ett tal mellan 0 och 9
```

För att generera ett slumpmässigt tal med decimaler, kan du använda `rand-nth`, som tar ett argument för antalet decimaler som du vill ha. Till exempel:

```Clojure
(rand-nth 5) ; genererar ett tal med 5 decimaler
```

# Djupdykning

För att förstå hur funktionen `rand` fungerar, måste vi förstå att den egentligen använder en pseudoslumpgenerator, vilket innebär att den genererar tal som är baserade på en matematisk algoritm istället för att vara helt slumpmässiga. Detta gör den eftersom det är mycket svårt för en dator att producera verkligt slumpmässiga tal.

Clojure använder en variant av Mersenne Twister-algoritmen för att generera pseudoslumpmässiga tal. Detta är en väldigt effektiv algoritm som producerar högkvalitativa tal, men den är inte helt slumpmässig och bör inte användas för kryptografiska ändamål.

# Se även

- [The Clojure Cheatsheet](https://clojure.org/api/cheatsheet)
- [Mersenne Twister in Wikipedia (Swedish)](https://sv.wikipedia.org/wiki/Mersenne_Twister)