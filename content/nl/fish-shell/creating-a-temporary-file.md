---
title:                "Een tijdelijk bestand aanmaken"
aliases:
- nl/fish-shell/creating-a-temporary-file.md
date:                  2024-01-28T21:58:09.899253-07:00
model:                 gpt-4-0125-preview
simple_title:         "Een tijdelijk bestand aanmaken"

tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/nl/fish-shell/creating-a-temporary-file.md"
changelog:
  - 2024-01-28, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Wat & Waarom?

Een tijdelijk bestand maken betekent een bestand maken voor kortdurend gebruik. Programmeurs doen dit om gegevens op te slaan die alleen nodig zijn tijdens de uitvoering van een programma, zoals tussenresultaten of om een schone staat te waarborgen zonder de permanente opslag te vervuilen.

## Hoe te:

In Fish Shell kun je een tijdelijk bestand maken met `mktemp`. Hier is een snel voorbeeld:

```fish
set tempfile (mktemp)
echo "Hallo, tijdelijke wereld!" > $tempfile
cat $tempfile
rm $tempfile
```

En je zult zoiets zien als dit:

```shell
Hallo, tijdelijke wereld!
```

Dit maakt een tijdelijk bestand, schrijft er een regel naar toe, toont de inhoud en verwijdert vervolgens het bestand.

## Diepgaand

Vroeger werden tijdelijke bestanden vaak handmatig aangemaakt, wat leidde tot mogelijke naamconflicten en beveiligingsproblemen. `mktemp` schiet te hulp! Dit commando maakt een bestand met een unieke naam, waardoor het risico op bestandsbotsing vermindert.

Alternatieve methoden zijn schrijven naar `/dev/shm` op Linux of het gebruik van op geheugen gebaseerde bestandssystemen. Echter, deze methoden zijn niet zo draagbaar als `mktemp`.

Wat betreft de levensduur van tijdelijke bestanden, is het van vitaal belang te onthouden dat ze moeten worden verwijderd door het programma dat ze heeft gemaakt. Dit zorgt ervoor dat er geen overgebleven bestanden zijn die systeemruimte verbruiken. In sommige systemen wordt de map `/tmp` bij het opstarten geleegd, maar je moet niet op dit gedrag vertrouwen voor opruiming.

## Zie Ook

- Fish Shell Documentatie: [https://fishshell.com/docs/current/](https://fishshell.com/docs/current/)
- `mktemp` Handleiding: [https://www.gnu.org/software/autogen/mktemp.html](https://www.gnu.org/software/autogen/mktemp.html)
- Bestandssysteem Hiërarchie Standaard: [https://refspecs.linuxfoundation.org/FHS_3.0/fhs/index.html](https://refspecs.linuxfoundation.org/FHS_3.0/fhs/index.html)
