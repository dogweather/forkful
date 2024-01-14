---
title:                "Bash: Utskrift av felsökningsutmatning"
simple_title:         "Utskrift av felsökningsutmatning"
programming_language: "Bash"
category:             "Bash"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/bash/printing-debug-output.md"
---

{{< edit_this_page >}}

## Varför

Att skriva Bash-kod kan vara en utmanande uppgift och ofta stöter man på problem längs vägen. Att lägga till debugutmatning i koden kan hjälpa till att hitta fel och lösa dem snabbare. Det kan också ge en bättre förståelse för hur koden fungerar och därmed förbättra kvaliteten på slutprodukten.

## Hur man gör det

Att lägga till debugutmatning i din Bash-kod är en enkel process. Det finns flera sätt att göra det, men här är några enkla sätt att komma igång:

1. Använd `echo`-kommandot för att skriva ut variabler i olika delar av koden.
```Bash
echo "Variabeln är $var"
```
2. Använd `set -x` för att aktivera debug-läget som kommer att skriva ut varje rad i koden när den körs.
```Bash
set -x
# kod här
set +x # för att inaktivera debug-läget igen
```
3. Använd `read` för att vänta på användarens input och skriva ut det i koden.
```Bash
read -p "Ange ett nummer: " num
echo "Du angav: $num"
```

## Djupdykning

Att lägga till debugutmatning kan vara mycket hjälpsamt men det kan också leda till större filstorlekar och onödigt utskrift. Här är några tips för att optimera din debugutmatning:

- Använd `>/dev/null` för att dölja output från vissa kommandon såsom `curl` eller `grep`.
```Bash
curl example.com > /dev/null # skriver ut resultaten på skärmen men de ignoreras
```

- Använd `echo -n` för att hålla debug-utmatningen på samma rad som kodraden.
```Bash
echo -n "Om du kan läsa detta är koden körd"
```

- Använd `>&2` för att skriva ut felmeddelanden på standard error istället för standard output.
```Bash
(( 1/0 )) 2>&1 # skriver ut felmeddelandet på standard error istället för skärmen
```

## Se även

Här är några användbara artiklar och resurser för att lära dig mer om att lägga till debugutmatning i din Bash-kod:

- [Debug Bash Scripts Like a Pro](https://www.enterprisedb.com/postgres-tutorials/debug-bash-scripts-pro)
- [Debugging Bash scripts](https://linuxconfig.org/debugging-bash-scripts)
- [Bash Debugging - Linux Journal](https://www.linuxjournal.com/content/bash-debugging-linux-journal)