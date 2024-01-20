---
title:                "Laste ned en nettside"
html_title:           "Elixir: Laste ned en nettside"
simple_title:         "Laste ned en nettside"
programming_language: "Bash"
category:             "Bash"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/bash/downloading-a-web-page.md"
---

{{< edit_this_page >}}

# Nedlasting av en nettside med Bash-skripting

## Hva & Hvorfor?

Nedlasting av en nettside refererer til prosessen med å lagre data fra en nettside på din lokale enhet. Programmerere gjør dette for å ha tilgang til nettinnholdet offline, for dataanalyse, eller for å overvåke endringer på nettsiden.

## Hvordan Gjøre Det:

```Bash
#!/bin/bash
# En enkel Bash-skript for å laste ned en nettside

url="https://www.example.com"
wget $url
```

Kjør skriptet. Du vil se noe som dette i terminalen din:

```Bash
$ ./download.sh
--2022-06-20 14:20:30--  https://www.example.com/
Resolving www.example.com... 93.184.216.34, 2606:2800:220:1:248:1893:25c8:1946
Connecting to www.example.com|93.184.216.34|:443... connected.
HTTP request sent, awaiting response... 200 OK
Length: unspecified [text/html]
Saving to: ‘index.html’

index.html                [ <=>                ]  1.26K  --.-KB/s    in 0s
      
2022-06-20 14:20:30 (29.3 MB/s) - ‘index.html’ saved [1294]
```

## Dyp Dykk

Til alle tider, fra de tidligste dagene av webben, har det å laste ned nettsider vært en nøkkeloperasjon for å hente informasjon. Den første versjonen av `wget` ble utgitt allerede i 1996.

Alternativer til `wget` inkluderer `curl`, en annen kommandolinjeverktøy for dataoverføring, og `requests`-biblioteket for Python. Valg av verktøy avhenger av behovene til din bestemte bruksak.

Når det gjelder gjennomføringen, sender `wget` en HTTP GET-forespørsel til den angitte URL-en, mottar svaret fra serveren, og deretter lagrer den responsdataene i en fil på din lokale enhet.

## Se Også

1. `wget` man-siden: [https://www.gnu.org/software/wget/manual/wget.html](https://www.gnu.org/software/wget/manual/wget.html)
2. `curl` hjemmeside: [https://curl.se](https://curl.se)
3. `requests` dokumentasjon: [https://docs.python-requests.org/en/latest/](https://docs.python-requests.org/en/latest/)