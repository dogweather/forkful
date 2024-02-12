---
title:                "Skriva till standardfel"
date:                  2024-02-03T19:34:24.297928-07:00
model:                 gpt-4-0125-preview
simple_title:         "Skriva till standardfel"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/python/writing-to-standard-error.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Vad & Varför?
Att skriva till standardfel i Python handlar om att rikta ditt programs felmeddelanden eller diagnostik till felströmmen (`stderr`), separat från standardutmatningen (`stdout`). Programmerare gör detta för att skilja normala programutskrifter från felmeddelanden, vilket underlättar felsökning och logganalys.

## Hur gör man:
### Använda `sys.stderr`
Pythons inbyggda `sys`-modul tillåter explicit skrivning till `stderr`. Detta tillvägagångssätt är okomplicerat för enkla felmeddelanden eller diagnostik.

```python
import sys

sys.stderr.write('Fel: Något gick fel.\n')
```
Exempel på utdata (till stderr):
```
Fel: Något gick fel.
```

### Använda `print`-funktionen
Pythons `print`-funktion kan omdirigera sin utskrift till `stderr` genom att specificera `file`-parametern. Denna metod är användbar för att utnyttja `print`:s användarvänlighet samtidigt som man hanterar felmeddelanden.
```python
from sys import stderr

print('Fel: Fel i modulen.', file=stderr)
```
Exempel på utdata (till stderr):
```
Fel: Fel i modulen.
```

### Använda `logging`-modulen
För en mer omfattande lösning kan Pythons `logging`-modul rikta meddelanden till `stderr` och mycket mer, såsom att skriva till en fil eller anpassa meddelandeformat. Denna metod är bäst för applikationer som kräver olika nivåer av loggning, meddelandeformatering eller destinationer.
```python
import logging

logging.basicConfig(level=logging.WARNING)
logger = logging.getLogger(__name__)

logger.error('Fel: Databasanslutning misslyckades.')
```
Exempel på utdata (till stderr):
```
ERROR:__main__:Fel: Databasanslutning misslyckades.
```

### Tredjepartsbibliotek: `loguru`
`loguru` är ett populärt tredjepartsbibliotek som förenklar loggning i Python-applikationer. Det dirigerar automatiskt fel till `stderr`, bland andra funktioner.

För att använda `loguru`, installera det först via pip:
```shell
pip install loguru
```

Inkorporera sedan det i ditt Python-skript enligt följande:
```python
from loguru import logger

logger.error('Fel: Misslyckades med att öppna fil.')
```
Exempel på utdata (till stderr):
```
2023-04-05 12:00:00.000 | ERROR    | __main__:<module>:6 - Fel: Misslyckades med att öppna fil.
```
