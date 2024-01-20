---
title:                "Att skapa en tillfällig fil"
html_title:           "Bash: Att skapa en tillfällig fil"
simple_title:         "Att skapa en tillfällig fil"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/typescript/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## Vad och Varför?
Skapandet av en temporär fil innebär att programmerare skapar en fil för tillfällig användning. Vi gör detta för att lagra data som är avsedd att användas kortsiktigt och sedan släppas, vilket kan minska belastningen på minnet.

## Hur gör man:
Hantera temporära filer i TypeScript kan bli klättrigt, men här är ett exempel:
   
```TypeScript
import { fileSync } from 'tmp-promise';

async function createTempFile() {
    const { path, cleanup } = fileSync();
    console.log('Temporär fil skapad:', path);

    // Rensa upp temporära filen när du är klar
    await cleanup();
}

createTempFile().catch(console.error);

```
Detta skapar en temporär fil och skriver ut stigen till den. När du är klar med filen kan du bara kalla `cleanup` för att ta bort den.

## Djup Dykning
Historiskt sett används temporära filer i datorprogram för att lagra information som programmet behöver tillgå till endast en kort tid. Alternativen till temporära filer inkluderar användning av minne (RAM), vilket kan vara snabbare men också mer kostsamt.

När det gäller typiska implementationer, använder vi ofta bibliotek som `tmp-promise` eftersom det hanterar mycket av komplexiteten åt oss. Dock, det finns något att säga för att förstå bakomliggande tekniker som systemanrop och filhämtning.

## Se även
För mer information, här är några länkar:

1. [Officiell dokumentation för typskrift](https://www.typescriptlang.org/docs/)
2. [tmp-promise på npm](https://www.npmjs.com/package/tmp-promise)
3. [Historien om temporära filer](https://en.wikipedia.org/wiki/Temporary_folder) 

Note that TypeError can be thrown if the cleanup function is called while the file is still being used. Make sure you always finish operations before you dispose of the file.