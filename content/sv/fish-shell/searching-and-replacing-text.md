---
title:                "Sökning och ersättning av text"
html_title:           "Fish Shell: Sökning och ersättning av text"
simple_title:         "Sökning och ersättning av text"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/fish-shell/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## Vad & Varför? 
Sökning och ersättning av text är en vanlig uppgift för programmerare. Det är en process där vi letar efter ett specifikt mönster i en text och ersätter det med ett annat. Detta är användbart för att snabbt ändra flera förekomster av samma text samt göra storskaliga redigeringar på en gång.

## Hur man: 
```Fish Shell``` har inbyggda funktioner för att hantera sökning och ersättning av text. För att ersätta all förekomst av ett visst mönster med ett annat används kommandot ```string replace``` som i exemplet nedan: 
```
$ string replace "old_text" "new_text" file.txt 
```
Detta kommando söker igenom ```file.txt``` och ersätter alla förekomster av "old_text" med "new_text". I slutet av kommandot kan du lägga till flaggan ```-n``` för att visa vilka rader som skulle ändras utan att faktiskt ändra dem.

## Fördjupning: 
Historiskt sett har ```sed``` (Stream Editor) varit ett populärt verktyg för att söka och ersätta text i kommandoraden. ```Sed``` kan användas tillsammans med ```Fish Shell``` men är mer lämpat för andra shellobjekt som Bash. Andra alternativ inkluderar verktyg som ```awk``` och ```perl```. 

Detaljerade instruktioner och användningsområden för sökning och ersättning med ```Fish Shell``` finns i dokumentationen: [https://fishshell.com/docs/current/cmds/string.html#string-replace](https://fishshell.com/docs/current/cmds/string.html#string-replace). 

## Se även: 
Officiell hemsida för ```Fish Shell```: [https://fishshell.com/](https://fishshell.com/) 
Dokumentation för sökning och ersättning med ```Fish Shell```: [https://fishshell.com/docs/current/cmds/string.html](https://fishshell.com/docs/current/cmds/string.html)