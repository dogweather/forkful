---
title:                "Een datum converteren naar een string"
date:                  2024-01-28T21:57:14.483466-07:00
model:                 gpt-4-0125-preview
simple_title:         "Een datum converteren naar een string"
programming_language: "Bash"
category:             "Bash"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/nl/bash/converting-a-date-into-a-string.md"
changelog:
  - 2024-01-28, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Wat & Waarom?
Een datum omzetten naar een string betekent dat je het omzet in tekst die de datum vertegenwoordigt. We doen dit om datums te formatteren voor mensen om te lezen of om ze voor te bereiden voor opslag in tekstformaten zoals CSV of JSON.

## Hoe:
Hieronder staan voorbeelden van hoe je een datum naar een string kunt omzetten in Bash:

```Bash
# Toon de huidige datum en tijd in het standaardformaat
echo $(date)

# Aangepast formaat: YYYY-MM-DD
echo $(date '+%Y-%m-%d')

# Voeg de tijd toe
echo $(date '+%Y-%m-%d %H:%M:%S')

# Zet een bestaande datum om
bestaande_datum='2023-03-17 08:00:00'
date -d "$bestaande_datum" '+%A, %B %d, %Y'
```
Voorbeelduitvoer voor de bovenstaande commando's:

```
Sat Mar 25 12:04:22 PDT 2023
2023-03-25
2023-03-25 12:04:22
Friday, March 17, 2023
```

## Diepe Duik
Unix-achtige systemen gebruiken al vroeg het `date` commando voor het afhandelen van datum en tijd. Zijn flexibiliteit maakt een reeks van formaten mogelijk, met dank aan opmaaksymbolen zoals `%Y` voor jaar en `%d` voor dag.

Er zijn alternatieven voor het `date` commando als je een andere technologiestack gebruikt. Bijvoorbeeld, Python heeft `datetime.strftime`, terwijl JavaScript het `Date` object biedt met methoden zoals `toLocaleDateString()`.

Wanneer je datums omzet in Bash, onthoud dat het `date` commando kan werken met de huidige timestamp van het systeem of een gegeven datum. Correcte afhandeling van tijdzones is ook cruciaal voor accurate datums.

## Zie Ook
- GNU coreutils 'date': https://www.gnu.org/software/coreutils/manual/html_node/date-invocation.html
- Geavanceerde Bash-Scripting Gids: https://tldp.org/LDP/abs/html/
- Opmaaksymbolen voor het date commando: https://man7.org/linux/man-pages/man1/date.1.html
