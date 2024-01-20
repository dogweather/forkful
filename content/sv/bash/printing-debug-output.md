---
title:                "Skriva ut felsökningsresultat"
html_title:           "Fish Shell: Skriva ut felsökningsresultat"
simple_title:         "Skriva ut felsökningsresultat"
programming_language: "Bash"
category:             "Bash"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/bash/printing-debug-output.md"
---

{{< edit_this_page >}}

## Vad och varför?

Att skriva ut debug-utdata ("debug output") inom programmering innebär att spåra och visa data under programmets exekvering. Programmers gör detta för att hitta och lösa fel snabbt.

## Så här gör du:

Här är några enkla exempel med utdata.

```Bash
#!/bin/bash

# Använda 'echo' för att skriva ut debug information
echo "Det här är en debug information"

# Använda '-x' flaggan vid Bash körning för att skriva ut varje kommando
bash -x script.sh

# Använda 'set -x' och 'set +x' för att börja och sluta debug utskrift
set -x
echo "Now you can see this"
set +x
```
## Djupdykning

Historisk sett har Bash alltid tillhandahållit debugningsverktyg. Alternativ till `echo`, `printf`-funktionen tillhandahåller mer formatkontroll. Om du vill ha ännu mer kontroll kan du använda `logger`-kommandot för att skicka meddelanden till systemets loggningssystem.

## Se också

För mer detaljerad information, se följande länkar:

1. [Advanced Bash-Scripting Guide](http://tldp.org/LDP/abs/html/debugging.html): En fullständig guide om Bash-skriptning, inklusive debug.

2. [Bash Debugging](https://www.gnu.org/software/bash/manual/html_node/The-Set-Builtin.html): Det officiella dokumentet om hur man använder `set -x` och `set +x`.
   
3. [Bash logger command](https://linux.die.net/man/1/logger): Manualen för `logger` kommandot.
   
Lycka till med din kodning!