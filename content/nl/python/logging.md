---
title:                "Logboekregistratie"
aliases:
- nl/python/logging.md
date:                  2024-01-28T22:03:09.449202-07:00
model:                 gpt-4-0125-preview
simple_title:         "Logboekregistratie"

tag:                  "Good Coding Practices"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/nl/python/logging.md"
changelog:
  - 2024-01-28, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Wat & Waarom?
Loggen is het proces van het vastleggen van applicatiegebeurtenissen terwijl een programma draait, waardoor een spoor van hints voor post-mortemanalyse en real-time monitoring wordt geboden. Programmeurs doen dit omdat het helpt bij het debuggen van problemen, het monitoren van prestaties, en het volgen van gebruikersacties voor beveiliging en analyse.

## Hoe te:
Python wordt geleverd met een ingebouwde module voor loggen. Hier is een basisopstelling:
```Python
import logging

# Basisconfiguratie van het loggen
logging.basicConfig(level=logging.INFO)

# Logberichten
logging.debug('Dit is een debugbericht')
logging.info('Informatie over wat je programma net deed')
logging.warning('Een waarschuwingsbericht')
logging.error('Er is een fout opgetreden')
logging.critical('Het programma kan niet herstellen!')
```
Wanneer je deze code uitvoert, zie je de volgende output (aangezien het standaardniveau WAARSCHUWING is, worden debug- en infomeldingen niet getoond):
```
WARNING:root:Een waarschuwingsbericht
ERROR:root:Er is een fout opgetreden
CRITICAL:root:Het programma kan niet herstellen!
```
Je kunt het loggen ook instellen om naar een bestand te schrijven in plaats van de console:
```Python
logging.basicConfig(filename='app.log', filemode='w', level=logging.INFO)
```
Nu worden je logs naar het bestand 'app.log' gericht.

## Diepgaande duik
Loggen bestaat al sinds de vroege dagen van programmeren, met systeemlogs als een van de oudste vormen van persistente opslag buiten daadwerkelijke bestanden die gegevens bevatten. Geschiedenis daargelaten, blijft het hoofdconcept van loggen in essentie ongewijzigd, hoewel de tools zijn geëvolueerd.

De `logging` module van Python is vrij krachtig en flexibel. Het stelt programmeurs in staat om verschillende logniveaus (DEBUG, INFO, WAARSCHUWING, FOUT, KRITIEK) in te stellen die kunnen helpen bij het categoriseren en filteren van logs. Het heeft een hiërarchisch loggersysteem, wat betekent dat je ouder-kindrelaties tussen loggers kunt hebben en berichten de keten op kunt sturen.

Alternatieven zijn onder meer bibliotheken van derden zoals Loguru of structlog, die verbeterde functies bieden en een eenvoudiger interface dan de ingebouwde logmodule. Ze kunnen mooiere output bieden, betere serialisatie van gestructureerde gegevens en intuïtievere manieren om met logconfiguratie om te gaan.

Wat betreft implementatie, wanneer je loggen instelt, is het belangrijk om dit eenmaal aan het begin van je applicatie te doen. Het configureren op een module-niveau wordt aanbevolen met `logging.getLogger(__name__)` om de beste praktijken van Python loggen te volgen.

Loggen zou de prestaties van een applicatie onder normale omstandigheden niet drastisch moeten beïnvloeden. Echter, zorg moet worden genomen met wat er gelogd wordt: overdreven uitgebreid loggen, vooral op DEBUG-niveaus, kan een applicatie vertragen en snel logbestandsopslag vullen.

## Zie Ook
Voor meer over Python's logmodule, bekijk de officiële Python log-kookboek voor enkele geweldige voorbeelden en beste praktijken: https://docs.python.org/3/howto/logging-cookbook.html

Voor een diepgaande blik op gestructureerd loggen en hoe het logs informatiever en gemakkelijker te analyseren kan maken, is Loguru goed gedocumenteerd: https://loguru.readthedocs.io

Overweeg ook om een kijkje te nemen bij de 12-factor app-methodologie, specifiek de sectie over logs voor de moderne kijk op app-loggen: https://12factor.net/logs
