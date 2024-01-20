---
title:                "Skriva till standardfel"
html_title:           "Arduino: Skriva till standardfel"
simple_title:         "Skriva till standardfel"
programming_language: "Python"
category:             "Python"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/python/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## What & Why?
Skriva till standardfel (stderr) hanterar felmeddelanden separat från huvudoutputen (stdout). Programmerare gör detta för att organisera loggar, felsöka, och hantera fel korrekt.

## How to:
Använd `sys.stderr.write()` för att skriva till stderr och `print()` med `file=sys.stderr` för att dirigera utskriften till stderr.

```Python
import sys

# Skriver direkt till stderr
sys.stderr.write('Det här är ett felmeddelande.\n')

# Använder print-funktionen för att skriva till stderr
print('Det här är också ett felmeddelande.', file=sys.stderr)
```

Sample output:
```
Det här är ett felmeddelande.
Det här är också ett felmeddelande.
```

## Deep Dive
Standard error, introducerad med Unix, separerar felmeddelanden från vanlig output. Alternativ inbegriper loggfiler och tredjepartsbibliotek. Python implementerar stderr via `sys`-modulen och dess användning påverkar inte programmets returvärde.

## See Also
- Python's official documentation for the sys module: https://docs.python.org/3/library/sys.html
- The Unix standard streams concept: https://en.wikipedia.org/wiki/Standard_streams
- Logging best practices: https://docs.python.org/3/howto/logging.html