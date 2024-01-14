---
title:    "TypeScript: Att kontrollera om en mapp finns"
keywords: ["TypeScript"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/sv/typescript/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## Varför

När man utvecklar en applikation eller ett program, kan det vara viktigt att kontrollera om en viss mapp existerar. Detta kan till exempel vara användbart när man ska spara filer eller läsa innehållet i en mapp.

## Så här gör du

För att kontrollera om en mapp existerar i ditt program i TypeScript, använder du dig av funktionen "existsSync" från modulen "fs". Här är ett exempel på hur du kan använda denna funktion:

```TypeScript
import { existsSync } from 'fs';

const directory = 'mapp1';

if (existsSync(directory)) {
  console.log('Mappen existerar.');
} else {
  console.log('Mappen existerar inte.');
}
```

I detta exempel så kontrollerar vi om mappen "mapp1" existerar. Om så är fallet skriver vi ut "Mappen existerar." i konsolen, annars skriver vi ut "Mappen existerar inte.".

## Djupdykning

När man använder funktionen "existsSync" för att kontrollera om en mapp existerar, så returneras ett booleskt värde. Detta betyder att funktionen antingen returnerar "true" eller "false" beroende på om mappen existerar eller inte.

Det är också viktigt att notera att "existsSync" funktionen blockerar programmet tills kontrollen är klar. Detta kan påverka prestandan hos ditt program om du använder det på många olika mappar.

## Se även

För mer information och exempel på hur du kan använda "existsSync" funktionen, besök följande länkar:

- [Node.js dokumentation för existsSync](https://nodejs.org/api/fs.html#fs_fs_exists_path_callback)
- [Exempelkod för hur man kontrollerar om en mapp existerar](https://stackabuse.com/checking-if-a-file-or-directory-exists-using-node-js/)
- [En djupare förståelse för hur "existsSync" fungerar](https://medium.com/@yoniweisbrod/understanding-node-js-39-s-fs-exists-ffffc741802f)