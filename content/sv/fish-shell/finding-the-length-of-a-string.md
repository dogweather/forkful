---
title:                "Hitta längden på en sträng"
html_title:           "Fish Shell: Hitta längden på en sträng"
simple_title:         "Hitta längden på en sträng"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/fish-shell/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## Varför

Att kunna hitta längden på en sträng är en viktig del av programmering, särskilt när du arbetar med textbaserade data. Det kan hjälpa dig att hantera textsträngar på ett mer effektivt sätt och lösa olika problem.

## Hur man gör det

För att hitta längden på en sträng i Fish Shell kan du använda inbyggda funktionen "string length":

```Fish Shell
set my_string "Hej världen!"

echo (string length $my_string)
```

Output: `13`

För att få längden på ett användardefinierat argument eller en inmatad sträng, kan du använda "count":

```Fish Shell
set my_input (read)

echo (count $my_input)
```

Output: `9`

## Djupdykning

När du använder "string length" returneras en värdet som motsvarar antalet tecken i en sträng. Detta inkluderar även mellanslag och specialtecken. Om du vill räkna antalet ord i en sträng kan du använda "count".

Funktionen "count" kan också användas för att hitta antalet karaktärer i en viss position i en sträng. Till exempel, om du vill få den första bokstaven i en sträng, kan du använda "count" tillsammans med "string sub":

```Fish Shell
set my_string "Hello there!"

echo (string sub (count $my_string) $my_string)
```

Output: `H`

## Se också

- [Fish Shell dokumentation](https://fishshell.com/docs/current/index.html)
- [Funktioner i Fish Shell](https://fishshell.com/docs/current/commands.html#functions)
- [Stränghantering i Bash Shell](https://linuxhint.com/strings_management_bash/)