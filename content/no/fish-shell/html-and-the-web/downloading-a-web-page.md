---
title:                "Nedlasting av en nettside"
aliases:
- /no/fish-shell/downloading-a-web-page.md
date:                  2024-01-20T17:44:10.514583-07:00
model:                 gpt-4-1106-preview
simple_title:         "Nedlasting av en nettside"

tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/fish-shell/downloading-a-web-page.md"
---

{{< edit_this_page >}}

## Hva & Hvorfor?
Nedlasting av en nettside betyr å hente HTML-koden fra en nettadresse til lokalt lagringsmedium. Programmerere gjør dette for å analysere innhold, teste nettsider eller automatisere datainnsamling.

## Hvordan:
```Fish Shell
# Last ned en nettside med curl
curl https://example.com -o example.html

# Sjekk ut resultatet
cat example.html
```

```Fish Shell
# Hvis du vil vise innholdet direkte
curl https://example.com
```

```Fish Shell
# Lagre nettsiden mer stilfullt med wget
wget -O fancy-example.html https://example.com
```

```Fish Shell
# Eller bruk wget for å laste ned hele nettsteder
wget --mirror --convert-links --adjust-extension --page-requisites --no-parent http://example.com
```

```Fish Shell
# Eksempel på output
<!doctype html>
<html>
<head>
    <title>Eksempel</title>
</head>
<body>
    <p>Dette er et eksempel på en HTML-side.</p>
</body>
</html>
```

## Dypdykk
Originally, nedlasting av webinnhold var en primitiv men kritisk oppgave for tidlige internett-brukere og utviklere. Verktøy som `wget` og `curl` ble opprettet for å lette denne prosessen. De ble tilgjengelige i UNIX og Linux-distribusjoner og er fortsatt essensielle i moderne skript og automatiseringsoppgaver. Alternativer som HTTP-klientbiblioteker i programmeringsspråk (for eksempel Python's `requests`, Javascript's `axios`) tilbyr kraftigere og mer fleksibelt funksjonalitet i større programmer. Det underliggende prinsippet er det samme - sende en HTTP-forespørsel og lagre svaret.

Mens `wget` og `curl` kan virke overlappende, har de sine særegenheter. `curl` støtter flere protokoller og er ofte innebygd i programmeringsspråk for HTTP-forespørsler. `wget` er mer fokusert på nedlasting av filer og kan laste ned rekursivt, noe som er ideelt for å speile nettsteder.

For Fish Shell-brukere, klippe og lime disse kommandoene for nedlasting er ganske rett frem. Men Fishs syntaks kan variere subtilt fra andre shells (som bash), så vær klar over syntaksforskjeller før dykking i mer komplekse skripting.

## Se Også
- Fish Shell dokumentasjon: https://fishshell.com/docs/current/index.html
- `curl` dokumentasjon: https://curl.se/docs/manual.html
- `wget` dokumentasjon: https://www.gnu.org/software/wget/manual/wget.html
- Lær mer om HTTP-protokollen: https://developer.mozilla.org/en-US/docs/Web/HTTP
