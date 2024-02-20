---
date: 2024-01-20 17:43:49.848592-07:00
description: "Last ned en nettside betyr \xE5 hente HTML-innholdet fra en nettadresse.\
  \ Programmerere gj\xF8r dette for \xE5 analysere innholdet, teste tilgjengelighet,\
  \ eller\u2026"
lastmod: 2024-02-19 22:05:00.239407
model: gpt-4-1106-preview
summary: "Last ned en nettside betyr \xE5 hente HTML-innholdet fra en nettadresse.\
  \ Programmerere gj\xF8r dette for \xE5 analysere innholdet, teste tilgjengelighet,\
  \ eller\u2026"
title: Nedlasting av en nettside
---

{{< edit_this_page >}}

## What & Why?
Last ned en nettside betyr å hente HTML-innholdet fra en nettadresse. Programmerere gjør dette for å analysere innholdet, teste tilgjengelighet, eller automatisere datahenting.

## How to:
For å laste ned en nettside i Bash, kan du bruke `curl` eller `wget`. Her er en rask måte å gjøre det på:

```Bash
curl -o example.html https://www.example.com
```

Dette kommandoen lagrer innholdet på www.example.com til en fil kalt `example.html`.

Eller med `wget`:

```Bash
wget -O example.html https://www.example.com
```

`-O` (storbokstav O) angir output-filnavnet. Kjør kommandoen, og du får `example.html` med sidens innhold.

Output eksempel vil se slik ut:

```Bash
$ curl -o example.html https://www.example.com
# Ingen output betyr at nedlastingen var suksessfull.
```

```Bash
$ wget -O example.html https://www.example.com
--2023-04-08 12:00:00--  https://www.example.com/
Resolving www.example.com (www.example.com)... 93.184.216.34
Connecting to www.example.com (www.example.com)|93.184.216.34|:443... connected.
HTTP request sent, waiting for response... 200 OK
Length: unspecified [text/html]
Saving to: ‘example.html’

example.html           [ <=>                ]  1.70K  --.-KB/s    in 0s      

2023-04-08 12:00:01 (39.4 MB/s) - ‘example.html’ saved [1732]
```

## Deep Dive
Tidlig på 90-tallet gjorde Tim Berners-Lee de første skrittene for å skape WWW, og deretter ble verktøy som `curl` og `wget` utviklet for å jobbe med webinnhold via kommandolinjen. `curl` ble sluppet i 1997, og `wget` i 1996. De er nå uvurderlige for dagens scripting og programmeringsarbeid.

`curl` støtter fler protokoller og gir mer granulær kontroll, mens `wget` er mer fokusert på nedlasting. `curl` er som en "Swiss Army knife" for å jobbe med protokoller, `wget` er spesialisert for nedlasting og kan rekursivt laste ned hele nettsteder.

Bruk av disse verktøyene kan automatisere lasten av webdata og forenkle arbeidsflyt. For eksempel, å holde lokalt innhold synkronisert med en server, eller å skaffe live-data for applikasjoner.

## See Also
- curl's offisiell dokumentasjon: https://curl.se/docs/
- wget's offisiell dokumentasjon: https://www.gnu.org/software/wget/manual/wget.html
- En dypere forståelse av HTTP-protokollen: https://developer.mozilla.org/en-US/docs/Web/HTTP
- Bash scripting guide: https://www.gnu.org/software/bash/manual/
