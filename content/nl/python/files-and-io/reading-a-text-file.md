---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 22:05:02.390705-07:00
description: "Hoe: Het lezen van tekstbestanden is fundamenteel - en bestaat al sinds\
  \ de vroege dagen van het programmeren. Python's eenvoudige `open` functie heeft\u2026"
lastmod: '2024-04-05T22:51:03.244281-06:00'
model: gpt-4-0125-preview
summary: Het lezen van tekstbestanden is fundamenteel - en bestaat al sinds de vroege
  dagen van het programmeren.
title: Een tekstbestand lezen
weight: 22
---

## Hoe:
```python
# Het hele bestand in één keer lezen
with open('voorbeeld.txt', 'r') as bestand:
    inhoud = bestand.read()
    print(inhoud)

# Regel voor regel lezen
with open('voorbeeld.txt', 'r') as bestand:
    for regel in bestand:
        print(regel.strip())
```

Voorbeelduitvoer:
```
Dit is de eerste regel van het bestand.
En dit is de tweede regel.
```

## Diepgaand
Het lezen van tekstbestanden is fundamenteel - en bestaat al sinds de vroege dagen van het programmeren. Python's eenvoudige `open` functie heeft zijn wortels in de C-standaardbibliotheekfunctie `fopen`. Enkele alternatieven voor het lezen van tekstbestanden zijn het gebruik van bibliotheken zoals `pandas` voor CSV's of `json` voor JSON-bestanden. Intern, wanneer je een bestand leest, vraagt Python het besturingssysteem om een bestandsstroom te openen, wat lijkt op een lopende band die gegevens van het bestand naar je programma levert.

Voor grote bestanden, in plaats van `read()`, wat alles in het geheugen laadt, gebruik je `readline()` of itereer je over het bestandsobject met een `for` lus om één regel tegelijk af te handelen – efficiënt en geheugenvriendelijk. Terwijl `with open` de moderne aanpak is die bestanden automatisch sluit, kunnen oudere scripts `file.close()` gebruiken om dit handmatig te doen, al is dit foutgevoelig als er uitzonderingen gebeuren voordat de sluitaanroep plaatsvindt.

## Zie Ook
- Python Documentatie over IO: https://docs.python.org/3/tutorial/inputoutput.html#reading-and-writing-files
- Real Python Tutorial over Bestanden: https://realpython.com/read-write-files-python/
- Officiële Python Docs voor `open`: https://docs.python.org/3/library/functions.html#open
