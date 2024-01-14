---
title:    "Fish Shell: Utskrift av felsökningsutdata"
keywords: ["Fish Shell"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/sv/fish-shell/printing-debug-output.md"
---

{{< edit_this_page >}}

##Varför
Det är viktigt att kunna skriva ut debuggmeddelanden när man programmerar för att enklare kunna felsöka kod och hitta eventuella problem. Fish Shell har enkla och effektiva sätt att skriva ut debuggmeddelanden som kan hjälpa dig att hitta och lösa problemen i din kod.

##Så här gör du
För att skriva ut ett debuggmeddelande i Fish Shell kan du använda kommandot `echo` tillsammans med variabeln `$status` för att skriva ut eventuella felmeddelanden som kan uppstå i din kod. Se nedan för ett exempel:

```Fish Shell
echo "Debuggmeddelande: statusen är $status"
```

Detta kommer att skriva ut meddelandet "Debuggmeddelande: statusen är" följt av värdet på variabeln `$status`. På så sätt kan du enkelt se om något fel har uppstått och vad det eventuellt kan bero på.

##Djupdykning
Det finns flera olika sätt att skriva ut debuggmeddelanden i Fish Shell, beroende på vad du vill uppnå. Du kan till exempel använda kommandot `set -x` för att få Fish Shell att visa varje kommando som körs och dess resultat. Detta kan vara särskilt användbart om du försöker hitta en specifik bugg eller fel i din kod.

För mer avancerade debugginsatser kan du även använda Fish Shells inbyggda debuggverktyg såsom `stacktrace` för att få en detaljerad översikt över varje kommando och dess resultat i din kod.

##Se även
[Fish Shell - dokumentation](https://fishshell.com/docs/current/index.html)
[Fish Shell - felsökning](https://fishshell.com/docs/current/faq.html#debugging)
[Fish Shell - inbyggda verktyg](https://fishshell.com/docs/current/tutorial.html#using-fish-for-scripts-and-programs)