---
title:                "Hantering av fel"
aliases:
- /sv/fish-shell/handling-errors/
date:                  2024-01-26T00:52:27.213944-07:00
model:                 gpt-4-1106-preview
simple_title:         "Hantering av fel"

tag:                  "Good Coding Practices"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/fish-shell/handling-errors.md"
---

{{< edit_this_page >}}

## Vad & Varför?
Felhantering låter ditt skript hantera det oväntade på ett smidigt sätt. Vi gör det för att hantera fel utan att våra användares hår grånar.

## Hur gör man:
För att fånga fel i Fish, luta dig mot `status`-kommandot och villkorssatser. Anta att `ping` misslyckas; här är hur du upptäcker det:

```fish
ping -c 1 example.com
if not status is-success
    echo "Något fisigt hände med ping."
end
```

Exempelutdata om `ping` misslyckas:

```
Något fisigt hände med ping.
```

För att hantera en specifik felkod, använd `status --is`:

```fish
false
if status --is 1
    echo "Fångade ett fel med kod 1."
end
```

Exempelutdata:
```
Fångade ett fel med kod 1.
```

För en mer robust ansats, överväg att använda en funktion:

```fish
function try_ping
    ping -c 1 example.com
    or begin
        echo "Ping misslyckades med status $status"
        return 1
    end
end

try_ping
```

## Fördjupning
Felhantering i Fish matchar inte `try/catch`-paradigmet du kanske känner från högnivåspråk. Istället har du raka utgångsstatusar som tillhandahålls av `status`-kommandot.

Historiskt sett, i Unix-liknande system, innebär en utgångsstatus på `0` framgång, medan vilket icke-noll värde som helst indikerar ett fel, vilka vanligtvis speglar olika skäl till misslyckanden. Denna konvention används av de flesta kommandoradsverktyg och därmed även av Fish självt.

Alternativ till `status`-kontroller i Fish inkluderar signalhantering via `trap` i andra skal, men Fish föredrar mer uttalade statuskontroller, eftersom det är renare och mindre benäget för sidoeffekter.

Implementeringsmässigt förblir felhantering i Fish enkel men kraftfull, mycket tack vare dess icke-blockerande natur och betoning på tydlig syntax, som visas i exemplen. Felkoder samspelar snyggt med funktioner, vilket möjliggör modulär och lättläst felhantering.

## Se också
- Fish dokumentation om villkorssatser: https://fishshell.com/docs/current/language.html#conditionals
- Fish handledning om felhantering: https://fishshell.com/docs/current/tutorial.html#error-handling
