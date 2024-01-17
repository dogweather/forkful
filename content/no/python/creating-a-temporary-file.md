---
title:                "Opprettelse av en midlertidig fil"
html_title:           "Python: Opprettelse av en midlertidig fil"
simple_title:         "Opprettelse av en midlertidig fil"
programming_language: "Python"
category:             "Python"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/python/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## Hva & Hvorfor?
Å opprette en midlertidig fil betyr å opprette en midlertidig lagringsplass for data som skal brukes i et program. Dette gjøres for å holde programmet ryddig og organisert, og for å sikre at data ikke går tapt.

## Hvordan:
```python
import tempfile
with tempfile.TemporaryFile() as tmp:
    tmp.write(b'Dette er en midlertidig fil.')
    tmp.seek(0)
    data = tmp.read()
print(data)

# Eksempelutgang:
b'Dette er en midlertidig fil.'
```

## Dypdykk:
Opprettelsen av midlertidige filer har vært en viktig del av programmering siden tidlig på 1960-tallet. Alternativene til å opprette midlertidige filer inkluderer å lagre data i minnet eller å bruke permanente filer, men sistnevnte kan føre til rotete katalogstrukturer og potensiell datatap i tilfelle krasj. Midlertidige filer brukes også i operativsystemet for å lagre temporære systemfiler.

## Se også:
[Python's tempfile documentation](https://docs.python.org/3/library/tempfile.html)