---
title:                "Bash: Att skriva tester"
simple_title:         "Att skriva tester"
programming_language: "Bash"
category:             "Bash"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/bash/writing-tests.md"
---

{{< edit_this_page >}}

## Varför

Att skriva tester är en viktig del av att utveckla programvara. Det hjälper till att upptäcka buggar och fel i koden, vilket leder till en mer robust och tillförlitlig produkt. Det är även ett sätt att garantera att koden fungerar som den ska även vid framtida ändringar eller uppdateringar.

## Hur man gör

För att skriva tester i Bash, används kommando-kommandot `test` eller dess synonym `[` följt av ett uttryck eller villkor att utvärdera. Här är ett exempel på detta:

```Bash
#!/bin/bash

# Testa om ett tal är större än 10
tal=15

if [ $tal -gt 10 ]
then
    echo "Talet är större än 10."
fi
```

I detta exempel använder vi `if`-satsen för att kontrollera om villkoret att `tal` är större än 10 är sant. Om det är sant, skrivs meddelandet "Talet är större än 10" ut. Om villkoret inte är sant, så skrivs inget ut.

En annan användbar metod för att skriva tester är att använda `&&` och `||` operatorerna. Dessa används för att köra vissa kommandon bara om ett villkor är uppfyllt eller icke-uppfyllt.

```Bash
#!/bin/bash

# Kontrollera om katalogen "library" existerar, och om det gör det, gå in i den.
test -d library && cd library

# Kontrollera om filen "readme.md" existerar, och om det gör det, skriv ut dess innehåll.
test -f readme.md && cat readme.md
```

Här använder vi `test` för att kontrollera om en katalog eller fil existerar. Om den gör det, utförs det andra kommandot efter `&&`-tecknet.

## Djupdykning

Det finns många olika sätt att skriva tester i Bash-skript, och det bästa sättet att lära sig är att fortsätta öva och experimentera. Några tips för effektiva tester inkluderar att vara så specifik som möjligt i dina villkor så att du inte får falskt positiva resultat, och att använda `&&` och `||` på ett strategiskt sätt för att skriva mer kompakt kod.

Ett annat viktigt koncept är att använda variabler för att lagra värden som ska utvärderas i tester. Detta gör det lättare att ändra villkoren i framtiden utan att behöva ändra själva testet.

## Se även

- [Bash Test Command på Linuxize](https://linuxize.com/post/bash-test-command/)
- [Bash Manual om "test" kommandot](https://www.gnu.org/software/bash/manual/html_node/Bash-Conditional-Expressions.html)
- [Bash Scripting på Wikibooks](https://en.wikibooks.org/wiki/Bash_Shell_Scripting)