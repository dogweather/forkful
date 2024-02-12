---
title:                "Controleren of een directory bestaat"
aliases:
- /nl/bash/checking-if-a-directory-exists.md
date:                  2024-01-28T21:55:36.294188-07:00
model:                 gpt-4-0125-preview
simple_title:         "Controleren of een directory bestaat"

tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/nl/bash/checking-if-a-directory-exists.md"
changelog:
  - 2024-01-28, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Wat & Waarom?

Controleren of een directory bestaat is belangrijk om zeker te weten dat een map er is voordat je probeert er iets mee te doen, zoals bestanden lezen of nieuwe opslaan. Programmeurs doen dit om fouten te voorkomen, de stroom van hun scripts soepel te houden en situaties te hanteren wanneer dingen niet helemaal zijn waar ze zouden moeten zijn.

## Hoe:

Zo controleer je of een directory bestaat in Bash:

```Bash
if [ -d "/pad/naar/map" ]; then
  echo "Directory bestaat."
else
  echo "Directory bestaat niet."
fi
```

Voorbeelduitvoer als de directory bestaat:

```
Directory bestaat.
```

En als het niet bestaat:

```
Directory bestaat niet.
```

Ja, het is zo eenvoudig. Maar vergeet niet om `/pad/naar/map` te vervangen door het daadwerkelijke pad dat je controleert.

## Dieper Duiken

Lang geleden deden mensen min of meer hetzelfde, door commandolijntests te gebruiken vergelijkbaar met wat we vandaag de dag doen. Bash heeft altijd een ingebouwde manier gehad om bestanden en directories te controleren, omdat het een fundamentele behoefte is.

Nu, waarom `-d` en niet iets anders? In Bash test `-d` specifiek op de aanwezigheid van een directory. Er zijn ook andere tests, zoals `-f` voor bestanden of `-e` voor bestaan (bestanden of directories).

Soms zie je misschien:

```Bash
if [[ -d "/pad/naar/map" ]]; then
  # Dubbele haakjes voor een beetje meer moderne, robuuste aanpak.
fi
```

Of zelfs `&&` en `||` voor kortere notaties:

```Bash
[ -d "/pad/naar/map" ] && echo "Directory bestaat." || echo "Directory bestaat niet."
```

Wees echter voorzichtig - deze laatste kan misleidend zijn als `echo "Directory bestaat."` om een of andere reden faalt, dan zal `echo "Directory bestaat niet."` uitgevoerd worden, zelfs als de directory bestaat. Gebruik het met voorzichtigheid en begrip.

## Zie Ook

- **Bash Voorwaardelijke Expressies**: [GNU Bash Handleiding](https://www.gnu.org/software/bash/manual/html_node/Bash-Conditional-Expressions.html)
- **Bash Scripting Tutorial**: [Ryans Tutorials](https://ryanstutorials.net/bash-scripting-tutorial/)
- **Geavanceerde Bash Scripting Gids**: [Het Linux Documentatie Project](https://tldp.org/LDP/abs/html/)
