---
title:    "Fish Shell: Skriva till standardfel"
keywords: ["Fish Shell"]
---

{{< edit_this_page >}}

## Varför

Att skriva till standard error är en viktig del av att skriva kod i Fish Shell. Det är ett enkelt sätt att kommunicera med användaren och ge information om fel som kan uppstå under körningen av ditt program.

## Hur man gör det

För att skriva till standard error i Fish Shell kan du använda kommandot `echo` tillsammans med `&2` som argument. Till exempel:

```Fish Shell
echo "Detta är ett felmeddelande" &2
```

Output:

```
Detta är ett felmeddelande
```

## Djupdykning

I Fish Shell finns det en standard output som är kopplad till användarens terminal, och en standard error som är kopplad till eventuella fel eller varningar. Genom att skriva till standard error kan du separera dessa två typer av utdata och ge användaren en tydligare förståelse för vad som händer under körningen av ditt program.

Det finns också möjlighet att skriva till både standard output och standard error samtidigt genom att använda `|&` som en pipa, istället för bara `|` som används för att skriva till endast standard output. Till exempel:

```Fish Shell
echo "Detta kommer att visas på både standard output och standard error" |& cat
```

Output:

```
Detta kommer att visas på både standard output och standard error
```

Detta kan vara särskilt användbart för att felsöka ditt program och ge användaren så mycket information som möjligt.

## Se även

- [Fish Shell dokumentation om standard error](https://fishshell.com/docs/current/index.html#standard-error)
- [En guide till användbara Fish Shell-kommandon](https://dev.to/veeralpatel/how-to-be-a-fish-shell-zealot-3d58)