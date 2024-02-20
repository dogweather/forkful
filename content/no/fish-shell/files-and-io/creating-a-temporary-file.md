---
date: 2024-01-20 17:40:52.183395-07:00
description: "Temporary files er midlertidige lagringssteder for data som kun trengs\
  \ under kj\xF8ring av et script eller program. Disse brukes for \xE5 unng\xE5 \xE5\
  \ forstyrre\u2026"
lastmod: 2024-02-19 22:05:00.526674
model: gpt-4-1106-preview
summary: "Temporary files er midlertidige lagringssteder for data som kun trengs under\
  \ kj\xF8ring av et script eller program. Disse brukes for \xE5 unng\xE5 \xE5 forstyrre\u2026"
title: Opprette en midlertidig fil
---

{{< edit_this_page >}}

## What & Why?
Temporary files er midlertidige lagringssteder for data som kun trengs under kjøring av et script eller program. Disse brukes for å unngå å forstyrre andre filer og for å holde på data som kan være for stort eller sensitivt for å plasseres i minnet.

## How to:
Oppretting av en midlertidig fil i Fish Shell:

```Fish Shell
# Oppretter en midlertidig fil og tilordner filbanen til en variabel
set tmpfile (mktemp)

# Viser navnet på den midlertidige filen
echo $tmpfile

# Bruk '$tmpfile' til å referere til filen i scriptet ditt
# ...

# Slett den midlertidige filen når du er ferdig
rm $tmpfile
```

Eksempelutdata:

```
/tmp/tmp.ikJi8xQxFg
```

## Deep Dive
Før i tiden håndterte programmerere midlertidige filer manuelt, noe som var risikabelt fordi det lett kunne føre til datatap eller konflikter. I `mktemp`-kommandoen fikk Fish Shell (og de fleste Unix-lignende skall) en standard måte å skape unike, midlertidige filer på. Denne kommandoen minimerer risikoen for filkonflikter og sikkerhetsproblemer.

Alternativer til `mktemp` inkluderer manuell håndtering av midlertidige filer (stressfullt og risikabelt) eller bruk av RAM-disker (raskere, men begrenset av RAM-størrelsen).

For å forstå hvordan `mktemp` fungerer, må vi vite at kommandoen skaper en fil med et unikt navn i `/tmp`-katalogen. Den garanterer at filnavnet er unikt ved hver kjøring, noe som er kritisk for å forhindre overskriving og risiko for kollisjon i flerbrukersystemer.

## See Also
- Fish Shell Documentation: https://fishshell.com/docs/current/index.html
- Unix `mktemp` man page: https://man7.org/linux/man-pages/man1/mktemp.1.html
- Advanced Bash-Scripting Guide on temporary files: https://www.tldp.org/LDP/abs/html/tmpfiles.html
