---
title:                "Gleam: Kontrollera om en mapp finns"
simple_title:         "Kontrollera om en mapp finns"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/gleam/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## Varför
I den här bloggposten kommer vi att titta på hur man kontrollerar om en mapp existerar i Gleam-programmeringsspråket. Detta är en viktig färdighet för att hantera filer och organisera ditt program.

## Hur man gör
För att kontrollera om en mapp existerar i Gleam, använder vi funktionen `Filesystem.directory_exists?` som tillhandahålls av den inbyggda `Filesystem`-modulen. Vi behöver först importera modulen i vårt program med `import Filesystem`, och sedan kan vi använda funktionen enligt följande exempel:

```Gleam
let directory_name = "min_mapp"

if Filesystem.directory_exists?(directory_name) {
    // Kod som ska utföra om mappen existerar
} else {
    // Kod som ska utföras om mappen inte existerar
}
```

Om `directory_name`-mappen existerar kommer det första blocket av kod att utföras, och om den inte existerar kommer det andra blocket att utföras. Det är viktigt att notera att `Filesystem.directory_exists?` funktionen endast accepterar strängar som argument, så se till att konvertera eventuella andra datatyper till strängar innan du kör funktionen.

## Djupdykning
När vi använder `Filesystem.directory_exists?` funktionen, så använder vi systemets filsystemgränssnitt för att kontrollera om den angivna mappen existerar. Om du kör detta på en Linux-baserad maskin kommer den att använda `stat`-systemanropet, medan den på en Windows-maskin kommer att använda `GetFileAttributesW`-systemanropet.

Det är också viktigt att notera att `Filesystem.directory_exists?` endast kontrollerar exakt mappen i den sökvägen som du anger. Om du till exempel anger `min_mapp/undermapp` som sökväg, kommer funktionen att returnera `false` om `min_mapp` inte existerar, även om `undermapp` existerar där. För att kontrollera om undermappen existerar, skulle du behöva köra funktionen med `min_mapp` som argument.

## Se även
- Gleam Filesystem Modulen: https://gleam.run/modules/gleam_io
- Linux `stat` systemanrop: https://man7.org/linux/man-pages/man2/stat.2.html
- Windows `GetFileAttributesW` systemanrop: https://docs.microsoft.com/en-us/windows/win32/api/fileapi/nf-fileapi-getfileattributesw