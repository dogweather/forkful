---
title:    "Java: Skapa en temporär fil"
keywords: ["Java"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/sv/java/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## Varför
Skapandet av temporär fil är en viktig del av att programera i Java. Det är användbart när du behöver lagra data temporärt och sedan ta bort filen efter användning.

## Hur man gör
Skapandet av en temporär fil i Java kan göras genom att använda klassen "File" och dess "createTempFile" metod. Detta kan göras enkelt med hjälp av följande kod:

```Java
File tempFile = File.createTempFile("temp", ".txt");
System.out.println("Temporär fil skapad: " + tempFile.getAbsolutePath());
```

Koden ovan skapar en temporär fil med namnet "temp" och filtypen ".txt". Det unika numret som genereras automatiskt av metoden läggs till i slutet av filnamnet. Genom att använda "getAbsolutePath" kan man få den fullständiga sökvägen till den temporära fil som skapats.

### Exempelutskrift:
Temporär fil skapad: /var/folders/9s/w980lm6x0gvccm7z_6m698r00000gn/T/temp5464209513701357656.txt

## Djupdykning
När en temporär fil skapas, lagras den i operativsystemets temporära filsystem. Detta kan variera beroende på vilket operativsystem som används. I Windows lagras den vanligtvis i mappen "AppData/Local/Temp", medan den på MacOS lagras i mappen "/tmp".

Det är också möjligt att ange en specifik mapp där den temporära filen ska skapas genom att ange en sökväg i "createTempFile" metoden.

Det är viktigt att notera att skapandet av en temporär fil inte garanterar att den kommer att finnas kvar under programmets körning. Filen kan tas bort av operativsystemet när som helst, särskilt om det finns begränsningar i utrymmet för temporära filer på systemet.

## Se även
- Java-klassdokumentation för "File": https://docs.oracle.com/javase/8/docs/api/java/io/File.html
- Tutorial för att skapa temporära filer i Java: https://www.baeldung.com/java-temporary-file