---
title:                "Schrijven naar standaardfout"
aliases:
- nl/fish-shell/writing-to-standard-error.md
date:                  2024-01-28T22:13:15.531251-07:00
model:                 gpt-4-0125-preview
simple_title:         "Schrijven naar standaardfout"

tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/nl/fish-shell/writing-to-standard-error.md"
changelog:
  - 2024-01-28, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Wat & Waarom?

Schrijven naar standaardfout (stderr) stuurt foutmeldingen en diagnostiek los van de hoofdoutput. Programmeurs gebruiken dit om problemen te rapporteren zonder de reguliere gegevensstroom te verstoren.

## Hoe te:

Om naar stderr in Fish te schrijven, gebruik je `echo` met `>&2`:

```Fish Shell
echo "Fout: Er is iets misgegaan" >&2
```

De output zal niet zichtbaar zijn in de reguliere commando-output, maar zal zichtbaar zijn op de console of kan worden omgeleid naar een bestand:

```Fish Shell
echo "Fout: Er is iets misgegaan" >&2 > /dev/null
```

Dit commando onderdrukt de standaardoutput maar toont de foutmelding.

## Diepgaande duik

Vanaf het begin heeft Unix aparte stromen voor gegevens en fouten vastgesteld: stdout en stderr. Het scheiden ervan maakt zuivere gegevensverwerking mogelijk en onafhankelijke foutafhandeling. In Fish, net als in andere shells, is `>&2` een operator die de output naar stderr leidt. Alternatieven voor het signaleren van fouten zijn onder andere exitstatussen en aangepaste logmechanismen, maar directe schrijfacties naar stderr zijn eenvoudig en veelgebruikt. Als een shell ontworpen voor interactief gebruik, neemt Fish functies over van andere shells, inclusief deze stderr-conventie.

## Zie ook

- De Fish Shell-documentatie: [Gebruik van stderr](https://fishshell.com/docs/current/index.html#redirection)
- POSIX shell scripting richtlijnen, van toepassing op stderr-afhandeling: [GNU Bash-handleiding](https://www.gnu.org/software/bash/manual/)
