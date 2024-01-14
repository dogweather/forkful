---
title:                "Gleam: Utskrift av felsökningsutdata"
programming_language: "Gleam"
category:             "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/gleam/printing-debug-output.md"
---

{{< edit_this_page >}}

# Varför

Att kunna identifiera och lösa buggar i kod är en viktig del av programmering och kan vara en utmanande uppgift. Att skriva ut debug-utmatning är ett användbart verktyg för att kunna undersöka och förstå vad som händer i koden, och därmed underlätta felsökning.

## Så här gör du

För att skriva ut debug-utmatning i Gleam kan du använda funktionen `debug!()` som tar emot en parameter. Till exempel:

```
Gleam.debug!("Värdet på variabeln är:", variabel)
```

Denna kod kommer att skriva ut texten "Värdet på variabeln är:" följt av värdet på den angivna variabeln.

Du kan också skriva ut flera värden på samma gång genom att använda en lista som parameter:

```
Gleam.debug!("Värdena på variablerna är:", [variabel1, variabel2, variabel3])
```

Genom att använda debug-utmatning kan du få information om variabler, funktioner och uttryck i din kod, vilket kan hjälpa dig att upptäcka och åtgärda eventuella problem.

## Djupdykning

När man använder debug-utmatning är det viktigt att känna till att uttryck som skrivs ut kommer att påverka prestandan i din kod. Detta beror på att utmatningen kräver extra resurser för att kunna skrivas ut och visas på skärmen. Därför bör du bara använda debug-utmatning när det verkligen behövs, och ta bort den när du inte längre behöver den.

Det är också viktigt att inte använda debug-utmatning i produktionskoden eftersom den kan avslöja kritisk information om din kod och dina system som kan utnyttjas av obehöriga.

## Se även

Här är några användbara länkar för att lära dig mer om debug-utmatning i Gleam:

- [Dokumentation för debug funktionen i Gleam](https://gleam.run/documentation/stdlib/debug)
- [En guide till felsökning i Gleam](https://medium.com/swlondon-fel/felsökning-i-gleam-367a70e155f4)
- [Diskussion om användningen av debug-utmatning i olika scenarier](https://github.com/gleam-lang/gleam/issues/305)

Lycka till med debugging i Gleam!