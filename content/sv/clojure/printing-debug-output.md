---
title:    "Clojure: Utskrift av felsökningsutdata"
keywords: ["Clojure"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/sv/clojure/printing-debug-output.md"
---

{{< edit_this_page >}}

## Varför

Att skriva kod kan ibland vara förvirrande och felsökning kan vara en utmaning. Att använda "debug output", eller utskrift av data för att hjälpa till med felsökningen, kan vara ett värdefullt verktyg för att förstå vad som händer i koden. Detta kan spara tid och frustration när man försöker hitta och åtgärda fel.

## Hur man gör det

Att skriva ut data i Clojure är enkelt. Man använder funktionen `println` för att skriva ut en sträng, variabel, eller ett uttryck. Om vi till exempel har en variabel `x` med värdet 5, så kan vi skriva ut värdet på följande sätt:

```Clojure
(println x)
```

Output: 5

Vi kan också skriva ut flera variabler eller uttryck på samma gång genom att ha dem separerade med mellanslag:

```Clojure
(println "Värdet på x är:" x "och värdet på y är:" y)
```

Output: Värdet på x är: 5 och värdet på y är: 10

Vi kan också använda funktionen `prn` för att skriva ut en mer formaterad version av datatypen, till exempel en lista:

```Clojure
(prn [1 2 3])
```

Output: [1 2 3]

## Djupdykning

Det finns flera olika sätt att använda `println` och `prn` för att skriva ut data. En av de mest användbara är att använda dem tillsammans med `str` funktionen för att skapa en formaterad sträng som innehåller variabler och text:

```Clojure
(println (str "Jag har " x " äpplen." ))
```

Output: Jag har 5 äpplen.

Man kan också använda `print` funktionen för att skriva ut utan newline-tecken:

```Clojure
(print "Hej " "alla")
(print "!")
```

Output: Hej alla!

Vid utskrift av mer komplexa datastrukturer kan det vara hjälpsamt att använda funktionen `pprint` som formaterar outputen på ett mer läsbart sätt:

```Clojure
(pprint (range 10))
```

Output: 
(0 1 2 3 4 5 6
 7 8 9)

## Se även

- Officiell dokumentation för utskrift i Clojure: https://clojure.org/reference/evaluation#_print_output
- En guide för felsökning i Clojure: https://purelyfunctional.tv/guide/troubleshooting-clojure-problems/