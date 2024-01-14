---
title:                "Fish Shell: Skicka en http-förfrågan"
simple_title:         "Skicka en http-förfrågan"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/fish-shell/sending-an-http-request.md"
---

{{< edit_this_page >}}

## Varför

I Fish Shell finns det många inbyggda funktioner som kan underlätta ditt programmeringsarbete. En av dem är möjligheten att skicka HTTP-förfrågningar direkt från skallet. Här berättar vi varför du skulle vilja använda denna funktion och hur du kan göra det.

## Hur man gör

För att skicka en HTTP-förfrågning i Fish Shell, behöver du använda kommandot `eval` tillsammans med `curl`. Här är ett exempel på hur man skickar en GET-förfrågning till Google:

```Fish Shell
eval echo (curl -X GET "http://www.google.com")
```

Detta kommer att ge dig ett utmatat svar från Google, i det här fallet dess hemsida. Notera att `eval` behövs för att få utmatningen från `curl`-kommandot. Du kan också lägga till flaggor för att anpassa förfrågningen, till exempel:

```Fish Shell
eval echo (curl -H "Content-Type: application/json" -d '{"username": "john", "password": "1234"}' -X POST "http://www.example.com/login")
```

Det här skulle skicka en POST-förfrågning med en JSON-body till en inloggningssida på www.example.com. Du kan lägga till fler flaggor enligt behov för att skräddarsy din förfrågning.

## Djupdykning

Genom att använda `curl`-kommandot inuti `eval` i Fish Shell kan du skicka HTTP-förfrågningar till alla webbadresser som stödjer det. Det kan vara användbart för att testa API-funktioner, ladda ner data från en server eller helt enkelt hämta en webbplats.

En annan användbar funktion är möjligheten att lagra utmatningen från förfrågningen i en variabel. Detta kan göras genom att tilldela outputen från `curl` till en variabel efter `eval`, till exempel:

```Fish Shell
set output (eval echo (curl -X GET "http://www.google.com"))
```

Nu kan du använda variabeln `output` för att bearbeta och använda datan som returnerades från förfrågningen.

## Se även

- [Fish Shell dokumentation](https://fishshell.com/docs/current/cmds/eval.html)
- [Användbara Fish Shell-kommandon för programmerare](https://blog.faraday.io/10-fish-shell-commands/)
- [Tutorial: Skicka HTTP-förfrågningar med cURL](https://www.youtube.com/watch?v=5pv0IKnS03I)