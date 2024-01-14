---
title:    "Fish Shell: Sökning och ersättning av text"
keywords: ["Fish Shell"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/sv/fish-shell/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## Varför

Att söka och ersätta text är en vanlig uppgift inom programmering, särskilt om man arbetar med stora mängder data eller repetitiva uppgifter. Med hjälp av Fish Shell kan du enkelt automatisera dessa uppgifter och spara tid och energi.

## Hur man gör

För att söka och ersätta text i Fish Shell kan du använda kommandot `sed`. Detta kommando tar två argument - söksträngen och ersättningssträngen - och tillämpar det på en given fil eller input. Här är ett exempel på hur det kan se ut:

```
Fish Shell > sed 's/Hej/Hello/' textfil.txt
```

I detta exempel kommer alla instanser av "Hej" i textfilen att ersättas med "Hello". Om du istället vill göra en global sökning och ersättning i hela filen, kan du lägga till flaggan `g` för globalt:

```
Fish Shell > sed 's/Hej/Hello/g' textfil.txt
```

Du kan också använda regelbundna uttryck för att söka och ersätta text. Till exempel, om du vill ersätta alla siffror i en fil med "X", kan du använda följande kommando:

```
Fish Shell > sed 's/[0-9]/X/' textfil.txt
```

## Djupdykning

En viktig sak att notera är att `sed` inte ändrar den ursprungliga filen, utan skriver ut den ändrade versionen till standardoutput. Om du vill spara de ändrade resultaten i en ny fil, kan du använda operatören `>`:

```
Fish Shell > sed 's/Hej/Hello/' textfil.txt > ny_textfil.txt
```

Det finns också flera användbara flaggor som du kan använda med `sed` för att kontrollera utdata och andra inställningar. En av de mer användbara är flaggan `-i` som låter dig ändra den ursprungliga filen. Till exempel, om du vill ändra "Hej" till "Hello" i `textfil.txt` kan du skriva:

```
Fish Shell > sed -i 's/Hej/Hello/' textfil.txt
```

Detta kommer att ändra filen direkt utan att skriva ut någon output. Notera dock att denna flagga inte finns tillgänglig på alla system.

## Se även

- [Fish Shell dokumentation för `sed`](https://fishshell.com/docs/current/commands.html#sed)
- [En djupdykning i regelbundna uttryck](https://www.regular-expressions.info/)