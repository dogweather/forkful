---
title:                "Fish Shell: Kontrollera om en mapp finns"
programming_language: "Fish Shell"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/fish-shell/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## Varför
Att kontrollera om en mapp finns är en viktig del av Fish Shell programmering. Genom att lära dig hur detta fungerar kan du bli mer effektiv och strukturerad i ditt arbete.

## Hur man gör det
För att kontrollera om en mapp finns kan du använda kommandot `test`, tillsammans med flaggan `-d` för att indikera att vi söker efter en mapp. Sedan behöver du ange sökvägen för mappen du vill kontrollera.

```Fish Shell
test -d /home/username/mapp
```

Om mappen finns kommer detta kommando att returnera `true`, annars kommer det att returnera `false`.

## Deep Dive
För att förstå hur detta kommando fungerar kan vi titta närmare på dess syntax och funktionalitet. `test` är ett inbyggt kommando i Fish Shell som används för att utföra logiska tester. Flaggan `-d` står för "directory" och används för att indikera att vi söker efter en mapp. 

När `test`-kommandot utförs evalueras sökvägen och om den pekar till en mapp returneras värdet `true`. Om sökvägen inte finns eller pekar till en annan typ av fil, såsom en vanlig fil eller program, returneras värdet `false`.

Detta kommando är särskilt användbart inom shell programmering eftersom det ger möjlighet att strukturera och organisera arbetsflödet baserat på existensen av vissa mappar.

## Se även
- [Fish Shell dokumentation om test-kommandot](https://fishshell.com/docs/current/commands.html#test)
- [En mer detaljerad guide om att kontrollera om en mapp finns](https://www.cyberciti.biz/faq/howto-check-if-a-directory-exists-in-a-bash-shellscript)
- [Detaljerad information om Fish Shell](https://fishshell.com/docs/current/)