---
title:                "Gleam: Läsa en textfil"
programming_language: "Gleam"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/gleam/reading-a-text-file.md"
---

{{< edit_this_page >}}

## Varför

Att läsa en textfil är en viktig del av alla programmeringsprojekt. Det är ett sätt att få tillgång till data som kan användas för att bygga ett program eller analys av information. I denna bloggpost kommer vi att utforska hur man läser en textfil med hjälp av Gleam-programmeringsspråket.

## Hur man gör

För att läsa en textfil i Gleam, kan du använda funktionen `gleam/file.open`. Detta kommer att öppna filen för läsning och returnera en `Result` typ som antingen innehåller filens innehåll eller ett felmeddelande. Vi kan använda `ok_or_raise` funktionen för att hantera detta resultat.

```Gleam
let result = file.open("mitt_dokument.txt")
let content =
  ok_or_raise(result)
  // Filens innehåll returneras om resultatet var `Ok`
  // Annars kastas ett felmeddelande
```

Nu när vi har filens innehåll, kan vi göra vad vi vill med det. Vi kan till exempel skriva ut det till konsolen med hjälp av funktionen `io.print`.

```Gleam
io.print(content)
```

Det är viktigt att notera att filens innehåll kommer att returneras som en `String` typ. Om du vill omvandla det till en lista av tecken kan du använda funktionen `string.to_char_list`.

## Djupdykning

När du läser en textfil är det viktigt att tänka på hur innehållet är strukturerat. Om du till exempel vill dela upp innehållet baserat på radbrytningar, kan du använda funktionen `string.split` tillsammans med tecknet för radbrytning `"\n"`.

```Gleam
let lines = content |> string.split("\n")

// Nu har vi en lista av varje rad i filen, som vi kan arbeta med
for line in lines {
  io.print(line)
}
```

Det är även möjligt att läsa filen rad för rad med hjälp av `file.read_line` funktionen. Detta kan vara till hjälp om filen är mycket stor och du inte vill läsa hela innehållet på en gång.

## Se även

- Dokumentation för `gleam/file` modulen: [dokumentation](https://gleam.run/modules/gleam/file/latest/)
- Lista av Gleam tutorials på svenska: [svenska tutorials](https://gleam.run/docs/tutorials/sv/)