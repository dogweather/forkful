---
title:                "Een string interpoleren"
date:                  2024-01-28T22:01:54.284238-07:00
model:                 gpt-4-0125-preview
simple_title:         "Een string interpoleren"
programming_language: "Bash"
category:             "Bash"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/nl/bash/interpolating-a-string.md"
changelog:
  - 2024-01-28, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Wat & Waarom?
Stringinterpolatie stelt je in staat waarden in een string in te voegen. Het is handig voor het creëren van aangepaste berichten, het automatiseren van commando's en het scripten als een baas.

## Hoe te:
Bash strings werken goed samen met variabelen. Laat een variabele in een string vallen met enkele krullende haakjes, en je bent klaar.

```Bash
name="World"
greeting="Hallo, ${name}!"
echo $greeting
```

Uitvoer:
```
Hallo, Wereld!
```

Bash zegt, "Houd het flexibel." Verander `name`, en je groet volgt vanzelf.

```Bash
name="Bash Pros"
greeting="Hallo, ${name}!"
echo $greeting
```

Uitvoer:
```
Hallo, Bash Pros!
```

## Diepere Duik
Vroeger plakten programmeurs strings aan elkaar met concatenatie. Het werd rommelig. Stringinterpolatie kwam als een superheld om de code schoner en leesbaarder te maken.

Bash, in tegenstelling tot sommige andere talen, maakt zich niet druk—gewoon een dollarteken en wat haakjes. Andere talen doen het met speciale syntaxis of functies. In Bash gaat het allemaal om die haakjes en af en toe een escapeteken als je je chique voelt.

Enkele alternatieven? Zeker, je kunt concatenatie gebruiken of `echo` zonder haakjes als je niets complex doet. Maar waarom genoegen nemen?

Wat betreft implementatie, wanneer Bash `${}` ziet, pakt het de variabelewaarde en wisselt deze in, geen vragen gesteld. Dit zorgt ervoor dat wat je ziet (in je code) is wat je krijgt (in je uitvoer).

## Zie Ook
Voor meer over stringmagie:

- Bash Stringmanipulatie: https://www.gnu.org/software/bash/manual/html_node/Shell-Parameter-Expansion.html
- Geavanceerde Bash-Scriptinggids: https://tldp.org/LDP/abs/html/
- Stack Overflow (praktische voorbeelden voor problemen uit de echte wereld): https://stackoverflow.com/questions/tagged/bash
