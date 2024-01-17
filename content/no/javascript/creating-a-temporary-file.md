---
title:                "Lage en midlertidig fil"
html_title:           "Javascript: Lage en midlertidig fil"
simple_title:         "Lage en midlertidig fil"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/javascript/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

Hva & Hvorfor?

Opprettelse av midlertidige filer er en vanlig praksis for programvareutviklere. Dette er prosessen med å opprette en fil som midlertidig lagrer data eller informasjon under kjøring av et program. Dette gjøres vanligvis for å organisere og behandle data på en mer effektiv måte.

Hvordan:

```Javascript
// Opprett en midlertidig fil med Node.js
var fs = require('fs');
var tempFile = fs.writeFileSync('temp.txt', 'Dette er en midlertidig fil');

// Slett filen etter bruk
fs.unlinkSync('temp.txt');
```

Eksempelutgang:

En midlertidig fil, kalt "temp.txt", vil bli opprettet og inneholde teksten "Dette er en midlertidig fil". Deretter vil filen bli slettet ved hjelp av fs.unlinkSync() funksjonen.

Dypdykk:

Opprettelse av midlertidige filer har sine røtter fra tidlig på 1960-tallet da filsystemer ble utviklet. Denne teknikken var nyttig for å lage midlertidige kopier av data eller for midlertidig lagring av informasjon under kjøring av et program.

Alternativer:

Noen programmeringsspråk har innebygde funksjoner for å opprette midlertidige filer, som Java med File.createTempFile() eller Python med tempfile.mktemp(). Men i mange tilfeller kan det å bruke eksterne biblioteker eller moduler, som fs i Node.js, være enklere og mer fleksibelt.

Se også:

- https://nodejs.org/api/fs.html#fs_fs_writefilesync_file_data_options
- https://docs.python.org/3/library/tempfile.html
- https://docs.oracle.com/javase/7/docs/api/java/io/File.html#createTempFile()