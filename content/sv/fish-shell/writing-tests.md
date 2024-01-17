---
title:                "Skriva tester"
html_title:           "Fish Shell: Skriva tester"
simple_title:         "Skriva tester"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/fish-shell/writing-tests.md"
---

{{< edit_this_page >}}

## Vad & Varför?

Skrivande av tester (eng. writing tests) är en avgörande del av programmering. Det innebär att man skapar specifika delar av kod som kontrollerar och validerar att ens program fungerar som det ska. Detta är viktigt eftersom det ger en försäkran om att koden fungerar korrekt och hjälper till att upptäcka eventuella fel eller buggar innan de når produktionsfasen.

## Så här gör du:

**Skapa en testfil:** För att skapa en testfil i Fish Shell, skriver man ```touch test.fish``` i terminalen och trycker på Enter.

**Definiera en funktion:** I testfilen definierar man en funktion som ska testas. Till exempel: 

```
# Testar om summa-funktionen returnerar rätt värde

function summa -d "Summerar två tal"
  set summa $argv[1] + $argv[2]
  echo $summa
end
```

**Kör tester:** I testfilen kan man sedan köra olika tester för att se om funktionen fungerar som den ska. Till exempel:

```
# Testar om summa-funktionen returnerar rätt värde för två positiva tal
test (summa 2 3) = 5

# Testar om summa-funktionen returnerar rätt värde för två negativa tal
test (summa -2 -3) = -5

# Testar om summa-funktionen returnerar rätt värde för ett positivt och ett negativt tal
test (summa 2 -3) = -1

# Testar om summa-funktionen returnerar rätt värde för noll
test (summa 0 0) = 0
```

**Kör testfilen:** För att köra testfilen, skriver man ```fish test.fish``` i terminalen och trycker på Enter. Om alla tester går igenom kommer man inte få något svar, men om ett eller flera tester misslyckas kommer man få ett felmeddelande som berättar varför.

## Deep Dive

Om man vill läsa mer om writing tests så kan man titta på andra testramverk som till exempel [Bats](https://github.com/bats-core/bats-core) eller [ShUnit2](https://github.com/kward/shunit2). Det finns också andra testramverk som är mer specifika för Fish Shell, som till exempel [Fish-Assertion](https://github.com/fisherman/fish-assertion).

Förutom att testa funktioner kan man också skapa tester för olika scenarion, till exempel input eller output. Det finns även möjlighet att använda sig av Mocking för att "fejka" olika förutsättningar och se hur koden beter sig.

## Se också

- [Fish Shell dokumentationen om testing](https://fishshell.com/docs/current/tutorial.html#alternate-commands-and-testing)
- [The Art of Software Testing, 3rd Edition](https://www.amazon.com/Art-Software-Testing-Glenford-Myers/dp/0471358460) (eng.)