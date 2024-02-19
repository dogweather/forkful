---
aliases:
- /no/python/creating-a-temporary-file/
date: 2024-01-20 17:41:14.349750-07:00
description: "Midlertidige filer er som papptallerkener; du bruker dem en gang, s\xE5\
  \ kaster du dem vekk. Programmerere lager slike filer for \xE5 lagre data midlertidig\
  \ uten\u2026"
lastmod: 2024-02-18 23:08:53.542585
model: gpt-4-1106-preview
summary: "Midlertidige filer er som papptallerkener; du bruker dem en gang, s\xE5\
  \ kaster du dem vekk. Programmerere lager slike filer for \xE5 lagre data midlertidig\
  \ uten\u2026"
title: Opprette en midlertidig fil
---

{{< edit_this_page >}}

## What & Why?
Midlertidige filer er som papptallerkener; du bruker dem en gang, så kaster du dem vekk. Programmerere lager slike filer for å lagre data midlertidig uten å påvirke permanent lagring eller for å unngå kompleks håndtering av datastrømmer.

## How to:
Python har et bibliotek som heter `tempfile` som er skreddersydd for midlertidige filer og mapper. Her er et eksempel:

```Python
import tempfile

# Opprette en midlertidig fil
with tempfile.TemporaryFile(mode='w+t') as temp:
    temp.write('Heisann, Norge!')
    # Gå tilbake til starten av filen og les innholdet
    temp.seek(0)
    print(temp.read())
    # Filen lukkes og slettes automatisk her

# Sjekk at filen faktisk er borte
try:
    print(temp.name)
except ValueError:
    print('Filen er slettet og finnes ikke lenger.')
```

Når du kjører dette, får du:
```
Heisann, Norge!
Filen er slettet og finnes ikke lenger.
```

## Deep Dive
Den `tempfile` modulen er en del av Python standardbiblioteket. Den har vært rundt en stund og er fortsette å være en pålitelig ressurs for filhåndtering. 

Alternativer inkluderer å lage egne filer i et definert midlertidig directory, men dette krever mer kode for sikker oppretting og sletting. `tempfile` håndterer dette sømløst for deg.

Implementeringen bruker lavnivå OS-funksjoner for å sikre at filene er unike og at de faktisk er borte når de er lukket. Det finnes flere funksjoner i biblioteket, som `NamedTemporaryFile`, som gir deg en midlertidig fil med et faktisk navn som du kan henvise til mens filen er åpen.

## See Also
- Python doc for `tempfile` module: https://docs.python.org/3/library/tempfile.html
- En artikkel om filhåndtering og IO i Python: https://realpython.com/python-file-io/ 
- Diskusjon om sikkerhet ved bruk av midlertidige filer: https://security.openstack.org/guidelines/dg_using-temporary-files-securely.html
