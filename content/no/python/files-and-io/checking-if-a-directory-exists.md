---
title:                "Sjekker om en mappe eksisterer"
aliases: - /no/python/checking-if-a-directory-exists.md
date:                  2024-02-03T19:08:14.987121-07:00
model:                 gpt-4-0125-preview
simple_title:         "Sjekker om en mappe eksisterer"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/python/checking-if-a-directory-exists.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Hva & Hvorfor?
Å sjekke om en mappe eksisterer i Python handler om å verifisere tilstedeværelsen av en mappe i filsystemet før man utfører operasjoner som å lese eller skrive filer. Programmerere gjør dette for å unngå feil som `FileNotFoundError`, for å sikre at applikasjonen oppfører seg pålitelig og ikke krasjer når den prøver å interagere med mapper.

## Hvordan:
Python gir innebygde måter å sjekke for en katalogs eksistens ved hjelp av `os` og `pathlib` modulene. Her er eksempler for begge:

### Bruke `os` modulen
```python
import os

# Spesifiser mappens sti
dir_path = "/path/to/directory"

# Sjekk om mappen eksisterer
if os.path.isdir(dir_path):
    print(f"Mappen {dir_path} eksisterer.")
else:
    print(f"Mappen {dir_path} eksisterer ikke.")
```

### Bruke `pathlib` modulen
```python
from pathlib import Path

# Spesifiser mappens sti
dir_path = Path("/path/to/directory")

# Sjekk om mappen eksisterer
if dir_path.is_dir():
    print(f"Mappen {dir_path} eksisterer.")
else:
    print(f"Mappen {dir_path} eksisterer ikke.")
```

### Tredjeparts biblioteker
Selv om Pythons standardbibliotek er tilstrekkelig for å sjekke om en mappe eksisterer, kan biblioteker som `pathlib2` være alternativer for konsistens på tvers av Python-versjoner eller for ekstra funksjonalitet.

***Merk:*** Per de siste Python-versjonene, er `pathlib` robust nok for de fleste brukstilfeller, noe som gjør tredjeparts biblioteker mindre nødvendige for denne spesifikke oppgaven.
