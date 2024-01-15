---
title:                "Skriva tester"
html_title:           "Bash: Skriva tester"
simple_title:         "Skriva tester"
programming_language: "Bash"
category:             "Bash"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/bash/writing-tests.md"
---

{{< edit_this_page >}}

## Varför
Att skriva tester är ett viktigt steg i utvecklingsprocessen för att säkerställa att koden fungerar som förväntat och att eventuella buggar upptäcks och åtgärdas i ett tidigt skede. Det hjälper även till att förbättra kodens kvalitet och underlättar för teamarbetet.

## Så här gör du
För att börja skriva tester i Bash, används kommandot `assert`. Detta kommando tar in två parametrar, ett villkor och ett meddelande, och kommer att jämföra villkoret med det förväntade resultatet. Om villkoret är sant, kommer testet att passera, annars kommer det att misslyckas och det angivna meddelandet kommer att visas.

Här är ett exempel på hur man skriver ett enkelt test för en funktion som lägger ihop två tal:

```
Bash function add(a, b) {
  echo $(($a + $b))
}

assert "add 2 3" "5" 
```

I det här fallet tar vi in parametrarna 2 och 3 till funktionen `add` och förväntar oss resultatet 5. Om funktionen returnerar ett annat värde, kommer testet att misslyckas och meddelandet "5" kommer att visas.

## Djupdykning
Det finns flera olika sätt att skriva tester i Bash, beroende på vilken typ av test du vill göra. Det kan vara enkelt att skriva små enhetstester för enskilda funktioner, men det kan bli mer komplicerat när det kommer till integrations- eller systemtester.

En bra praxis när man skriver tester är att ha en separat fil för dem, så att de inte blandas med koden. Detta hjälper till att hålla koden ren och läsbar. Du kan även använda verktyg som `grep` eller `awk` för att läsa resultatet från testerna och se om det finns några misslyckade tester.

När det kommer till att välja vad du ska testa, är det viktigt att fokusera på de delar av koden som är mest kritiska eller som du vet har haft problem tidigare. Genom att prioritera dessa områden kan du säkerställa en högre kvalitet på koden.

## Se även
- [Bash Test Assertions](https://bash.cyberciti.biz/guide/If_structures_to_execute_code_based_on_a_condition)
- [Testing and Debugging in Bash](https://linuxhint.com/testing_and_debugging_bash/)