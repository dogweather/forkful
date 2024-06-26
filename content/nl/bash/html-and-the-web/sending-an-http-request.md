---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 22:08:04.920468-07:00
description: 'Hoe te: Bash kan tools zoals `curl` of `wget` gebruiken voor HTTP-verzoeken.
  Hier is een snel voorbeeld met `curl`.'
lastmod: '2024-03-13T22:44:50.979257-06:00'
model: gpt-4-0125-preview
summary: Bash kan tools zoals `curl` of `wget` gebruiken voor HTTP-verzoeken.
title: Een HTTP-verzoek verzenden
weight: 44
---

## Hoe te:
Bash kan tools zoals `curl` of `wget` gebruiken voor HTTP-verzoeken. Hier is een snel voorbeeld met `curl`.

```Bash
# Haal de inhoud van een webpagina op
curl https://example.com

# Post gegevens naar een server
curl -d "param1=value1&param2=value2" -X POST https://example.com/post-endpoint

# Voeg headers toe aan een GET-verzoek
curl -H "Content-Type: application/json" https://example.com
```

Voorbeeld `curl`-respons:

```
<!doctype html>
<html>
<head>
    <title>Voorbeeld Domein</title>
...
</html>
```

## Diepgaand
HTTP-verzoeken bestaan sinds begin jaren '90 en vormen de basis van de communicatie op het web. `curl` en `wget` zijn Unix-commandoregeltools die respectievelijk in 1996 en 1996 geïntroduceerd zijn voor netwerkverzoeken.

`wget` wordt typisch gebruikt voor het downloaden van bestanden, terwijl `curl` een breed scala aan protocollen kan afhandelen en meer functies biedt, waardoor het de voorkeurstool is voor het versturen van HTTP-verzoeken vanaf de commandoregel.

Het implementeren van een HTTP-verzoek met deze tools omvat het opstellen van de juiste verzoekheaders, methode (GET, POST, PUT, DELETE, enz.), en soms data payloads. Dit doen vanuit Bash-scripts maakt automatisering van interactie met webgebaseerde diensten mogelijk.

Alternatieve methoden voor het versturen van HTTP-verzoeken in scripts zijn onder andere het gebruik van scripttalen zoals Python met bibliotheken zoals `requests`, of het gebruik van tools zoals `httpie` voor een meer gebruiksvriendelijke interface.

## Zie Ook
- curl officiële site: https://curl.se/
- wget handleiding: https://www.gnu.org/software/wget/manual/wget.html
- HTTPie: https://httpie.io/
- De Bash Academy: https://www.bash.academy/
- W3C HTTP-specificaties: https://www.w3.org/Protocols/
