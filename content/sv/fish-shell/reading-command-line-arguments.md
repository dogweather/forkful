---
title:                "Fish Shell: Läsning av kommandoradsargument"
simple_title:         "Läsning av kommandoradsargument"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/fish-shell/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## Varför

Att kunna läsa och hantera kommandoradsargument kan vara en otroligt användbar färdighet inom programmering. Genom att behärska denna kunskap kan du effektivt interagera med program och kommandon via terminalen.

## Hur man gör

För att läsa och hantera kommandoradsargument i Fish Shell används variabeln `$argv`. Den innehåller en lista av alla argument som skickas med till programmet vid körning. För att få en översikt av alla argument kan vi använda `count`-kommandot tillsammans med `$argv`. Nedan följer ett exempel:

```Fish Shell
echo "Antal kommandoradsargument: (count $argv)"
```

Detta kommer att skriva ut på skärmen hur många argument som skickats med. För att få ut själva argumenten kan vi använda indexering på `$argv`. Till exempel, för att få ut det första argumentet skriver vi följande kod:

```Fish Shell
echo "Det första argumentet är: $argv[1]"
```

Vi kan också använda range-indexering för att få ut flera argument samtidigt. Till exempel, för att få ut de två första argumenten skriver vi:

```Fish Shell
echo $argv[1..2]
```

## Djupdykning

Det finns flera användbara funktioner inom Fish Shell som underlättar för hanteringen av kommandoradsargument. Här är några av dem:

- `contains($var, $val)`: Används för att kontrollera om en variabel innehåller ett visst värde.
- `abbr($val)`: Ger en "förkortning" av ett visst värde. Till exempel kan `abbr "fish"` ge "f".
- `sep($val)`: Används för att dela upp en sträng vid ett visst tecken eller mönster.

Det finns även möjlighet att använda "wildcards" (jokertecken) när man läser kommandoradsargument. Till exempel, om man bara vill få ut argument som börjar med ett visst tecken kan man använda följande:

```Fish Shell
echo $argv[\$arg[1] = 'a'*]
```

Denna kod kommer att ge ut alla argument som börjar med bokstaven "a".

Sammanfattningsvis är det viktigt att förstå hur man kan läsa och hantera kommandoradsargument i Fish Shell för att underlätta interaktion med terminalen och effektivisera programmeringsarbetet.

## Se även

- [Dokumentation om kommandoradsargument inom Fish Shell](https://fishshell.com/docs/current/commands.html#managing-command-line-arguments)
- [En tutorial på YouTube om manipulation av kommandoradsargument i Fish Shell](https://www.youtube.com/watch?v=SEJVsYPrY00)
- [En bloggpost om användbara Fish Shell-funktioner](https://medium.com/@gregce/fish-shell-functions-you-should-know-cee4ee27dd96)