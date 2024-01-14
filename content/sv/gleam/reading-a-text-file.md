---
title:                "Gleam: Läsning av en textfil"
simple_title:         "Läsning av en textfil"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/gleam/reading-a-text-file.md"
---

{{< edit_this_page >}}

## Varför
Att läsa en textfil är en grundläggande programmeringsfärdighet som kan vara användbar i många olika scenarier. Det kan till exempel vara användbart för att läsa in data, bearbeta den och sedan spara den i en annan fil.

## Hur man gör det
För att läsa en textfil i Gleam finns det några enkla steg att följa:

1. Öppna filen med `File.open` och ange filvägen som en sträng.
2. Använd `File.read_all` för att läsa hela filen som en sträng.
3. Om du vill läsa filen rad för rad, använd `File.read_lines` för att få en lista av alla rader. Du kan sedan iterera över listan och göra vad du vill med varje rad.

Här är ett exempel på hur man kan läsa en textfil med namnet "data.txt" och skriva ut dess innehåll:

```Gleam
File.open("data.txt", _) |> File.read_all |> std.outln
```

Detta kommer att skriva ut hela filens innehåll i terminalen.

## Djupdykning
Det finns flera sätt att läsa en textfil i Gleam, beroende på vilken typ av data du vill ha tillgång till. Om du till exempel bara vill ha en specifik del av filen kan du använda funktionen `File.read_slice` för att läsa en viss del av filen baserat på ett visst intervall. Om du behöver göra mer komplicerade beräkningar på filen kan du använda funktionen `File.read_stream` för att läsa filen bit för bit och göra dina beräkningar medan den läser filen i realtid.

## Se även
- [Officiell dokumentation för File-modulen i Gleam](https://gleam.run/documentation/standard-library/file/)
- [En tutorial om hur man läser och skriver textfiler i Gleam](https://dev.to/gleam_lang/how-to-read-and-write-files-in-gleam-5d3l)