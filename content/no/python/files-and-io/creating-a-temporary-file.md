---
date: 2024-01-20 17:41:14.349750-07:00
description: 'How to: Python har et bibliotek som heter `tempfile` som er skreddersydd
  for midlertidige filer og mapper. Her er et eksempel.'
lastmod: '2024-03-13T22:44:40.380359-06:00'
model: gpt-4-1106-preview
summary: Python har et bibliotek som heter `tempfile` som er skreddersydd for midlertidige
  filer og mapper.
title: Opprette en midlertidig fil
weight: 21
---

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
