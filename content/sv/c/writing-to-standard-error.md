---
title:    "C: Skriva till standardfel"
keywords: ["C"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/sv/c/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## Varför

Att skriva till standard error är en viktig del av C-programmering eftersom det gör det möjligt att skriva ut felmeddelanden och debugging information till en annan konsol istället för den vanliga utgången. Detta gör det enklare att hitta och åtgärda buggar i ditt program.

## Hur man gör det

För att skriva till standard error i C, använder du funktionen `fprintf()` och anger `stderr` som första parameter. Detta kan se ut så här:

```C
fprintf(stderr, "Ditt felmeddelande här\n");
```

Detta kommer att skriva ut ditt felmeddelande till standard error-konsolen istället för den vanliga utgången. Du kan också använda denna metod för att skriva ut annan debugging information, som till exempel värden på variabler.

## Djupdykning

När du skriver till standard error, är det viktigt att komma ihåg att stänga av standard output-konsolen (vanligtvis `stdout`) om det inte används. Annars kan ditt program skriva ut felmeddelanden både till standard output och standard error, vilket kan orsaka förvirring.

En annan viktig punkt att notera är att `stderr` är en global variabel som pekar på en `FILE`-struktur. Det är dock möjligt att peka på en annan `FILE`-struktur och använda den som standard error genom att använda funktionen `freopen()`. Men det är inte rekommenderat att göra detta eftersom det kan påverka ditt programs beteende.

## Se även

- [fopen() dokumentation](https://www.cplusplus.com/reference/cstdio/fopen/)
- [sprintf() dokumentation](https://www.cplusplus.com/reference/cstdio/sprintf/)
- [Debugging i C-programmering](https://www.programiz.com/c-programming/debugging)