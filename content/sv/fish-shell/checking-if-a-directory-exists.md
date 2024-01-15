---
title:                "Kontrollera om en mapp finns."
html_title:           "Fish Shell: Kontrollera om en mapp finns."
simple_title:         "Kontrollera om en mapp finns."
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/fish-shell/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## Varför

Att kontrollera om en mapp (eng. directory) existerar är ett vanligt behov när du skriver kod. Det kan till exempel vara användbart för att undvika fel om din programvara förväntar sig att en viss mapp finns.

## Så här

För att kontrollera om en mapp existerar kan du använda "test"-kommandot och ange sökvägen till mappen du vill kontrollera. Om mappen existerar kommer kommandot att returnera utgångsvärdet 0, vilket betyder att det inte föreligger något fel. Om mappen inte existerar kommer utgångsvärdet att vara 1, vilket betyder att det har uppstått ett fel.

```Fish Shell
test -d /path/to/directory
```

Om du vill göra något beroende på resultatet av kontrollen kan du använda ett "if"-uttryck. I följande kodexempel kontrollerar vi om mappen "documents" finns och skriver ut ett meddelande som svar.

```Fish Shell
if test -d documents
	echo "Mappen existerar!"
end
```

## Djupdykning

Om du vill utföra mer avancerade åtgärder efter att ha kontrollerat om en mapp existerar kan du använda dig av "if"-uttrycken i kombination med andra kommandon. Till exempel kan du använda "set -q" för att kontrollera om en variabel finns tillgänglig innan du utför en åtgärd, eller "and"-operatorn för att utföra flera kontroller i samma uttryck.

Se till att noga läsa dokumentationen för Fish Shell för att lära dig om alla tillgängliga kommandon och uttryck för att kontrollera om en mapp existerar.

## Se också

- [Fish Shell dokumentation](https://fishshell.com/docs/current/)
- [Snabbguide till Fish Shell](https://fishshell.com/docs/current/tutorial.html)
- [Testkommandot (i Fish Shell)](https://fishshell.com/docs/current/cmds/test.html)