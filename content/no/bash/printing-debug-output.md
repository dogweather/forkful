---
title:                "Skrive ut feilsøkingsdata"
date:                  2024-01-20T17:51:47.509789-07:00
model:                 gpt-4-1106-preview
simple_title:         "Skrive ut feilsøkingsdata"

category:             "Bash"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/bash/printing-debug-output.md"
---

{{< edit_this_page >}}

## Hva & Hvorfor?
Å skrive ut feilsøkingsdata i terminalen hjelper programmerere å forstå hva som foregår under hetta. Det er som å ha et kart når du navigerer i en ny by - essensielt for å finne ut hvor og hvordan ting går galt.

## Slik gjør du:
For enkel tekst:
```Bash
echo "Dette er en feilsøkingsmelding"
```

Med variabler:
```Bash
DEBUG="Feilsøkingsverdien er"
VALUE=42
echo "$DEBUG $VALUE"
```

Utskrift med betingelse:
```Bash
DEBUG_MODE=1
if [ "$DEBUG_MODE" -eq 1 ]; then
  echo "Feilsøking er på."
fi
```

## Dybdeplunge:
Å skrive ut feilsøkingsdata, kjent som 'logging', har eksistert så lenge programmering har. Tidlige programmerere brukte fysiske utskrifter eller lyspaneler for å spore operasjoner.

Alternativer inkluderer skreddersydde feilsøkingsverktøy som `gdb` for detaljert sporing, eller ‘logger’-kommandoen i Bash for å håndtere alvorlighetsnivåer.

Bash bruker standardfilstrømmer; standard output (`stdout`) for normal utskrift og standard error (`stderr`) for feilmeldinger. Dette lar deg omdirigere dem til filer eller andre kommandoer.

## Se også:
- Bash man-page for fler detaljer: `man bash`
- Advanced Bash-Scripting Guide: https://www.tldp.org/LDP/abs/html/
- The Unix `logger` command: https://linux.die.net/man/1/logger
