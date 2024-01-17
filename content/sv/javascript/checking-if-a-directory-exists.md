---
title:                "Kontroll av existensen för en katalog"
html_title:           "Javascript: Kontroll av existensen för en katalog"
simple_title:         "Kontroll av existensen för en katalog"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/javascript/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## Vad & Varför?
Koll på om en mapp finns är en process där en utvecklare avgör om en viss mapp existerar inom ett givet system. Detta kan vara användbart för att undvika felmeddelanden eller hantera filer på ett effektivt sätt.

## Hur man gör:
För att kontrollera om en mapp finns kan du använda dig av funktionen `existsSync` från paketet `fs` i Javascript. Nedan finns ett exempel på hur du kan implementera detta:

```Javascript
const fs = require('fs');

if (fs.existsSync('/path/to/directory')) {
  console.log('Mappen finns!');
} else {
  console.log('Mappen existerar inte.');
}
```

## Djupdykning:
I äldre versioner av Javascript användes funktionen `fs.exists` för att kontrollera om en mapp finns. Detta anses numera som föråldrat och rekommenderas inte för användning. Istället bör du använda `fs.existsSync` som returnerar ett booleskt värde. Andra alternativ för att kolla om en mapp finns är användning av `stat` eller `access` funktionerna från `fs` paketet.

## Se även:
- [fs.existsSync()](https://nodejs.org/api/fs.html#fs_fs_existssync_path)
- [fs.stat()](https://nodejs.org/api/fs.html#fs_fs_stat_path_options_callback)
- [fs.access()](https://nodejs.org/api/fs.html#fs_fs_access_path_mode_callback)