---
title:                "Het huidige datum ophalen"
aliases:
- /nl/python/getting-the-current-date.md
date:                  2024-01-28T22:01:26.486727-07:00
model:                 gpt-4-0125-preview
simple_title:         "Het huidige datum ophalen"

tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/nl/python/getting-the-current-date.md"
changelog:
  - 2024-01-28, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Wat & Waarom?
Het verkrijgen van de huidige datum in Python betekent het ophalen van de live datum van het systeem waarop het draait. Programmeurs doen dit voor het loggen, tijdstempels, of wanneer de datum van vandaag nodig is voor een gebruikersinterface of een rapport.

## Hoe:
Gebruik de `datetime` module. Het is eenvoudig:

```Python
from datetime import datetime

# Haal de huidige datum op
huidige_datum = datetime.now().date()

# Print het uit
print(huidige_datum)
```

Een voorbeelduitvoer kan er zo uitzien:

```
2023-04-12
```

Let op: De uitvoer hangt af van de dag waarop je de code uitvoert. Uiteraard.

## Diepgaande verkenning
De `datetime` module is niet drastisch veranderd over de recente Python-versies. Het maakt deel uit van de standaardbibliotheek van Python - een zorgeloze gereedschapskist voor het omgaan met datums en tijden. Alternatieven? Zeker, er is `time`, maar dat is ruwer. Voor het zware werk kijkt de wereld naar `dateutil` en `arrow`, maar voor gewoon de datum van vandaag? Blijf bij `datetime`.

Onder de motorkap vist `datetime.now()` het huidige moment op volgens de tijdsinstellingen van je computer. Om tijdzonebewust te zijn, zou je bijvoorbeeld `datetime.now(timezone.utc)` gebruiken. Historisch gezien is omgaan met tijdzones een hoofdpijn gebleken, dus overweeg altijd locatie en zomertijd als het essentieel is.

Voor een snelle datum zonder de tijdstempel - zoals het maken van een bestand met de datum van vandaag in de naam - geeft `datetime.now().date()` je precies dat: een datumobject, dat jaar, maand en dag bevat.

## Zie ook
- Officiële Python-documentatie over `datetime`: https://docs.python.org/3/library/datetime.html
- `arrow` voor meer complexe datum-/tijdbehandeling: https://arrow.readthedocs.io
- `dateutil`, vanwege tijdzones: https://dateutil.readthedocs.io
- De tijdsinstellingen van je nederige pc omdat, nou ja, dat is waar Python eerst kijkt.
