---
title:                "Läsa en textfil"
html_title:           "Gleam: Läsa en textfil"
simple_title:         "Läsa en textfil"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/gleam/reading-a-text-file.md"
---

{{< edit_this_page >}}

##Varför
Att läsa en textfil är en väsentlig del av programmering, särskilt när man arbetar med stora mängder data. Genom att läsa en textfil kan du snabbt få tillgång till information och använda den för att utföra olika åtgärder.

##Så här gör du
För att läsa en textfil i Gleam, använder du funktionen `File` tillsammans med `open_read()`-funktionen för att öppna och läsa filen. Sedan kan du använda en `while`-loop för att loopa igenom filen och öppna den för läsning. Här är ett exempel på hur du kan läsa en textfil och skriva ut dess innehåll i terminalen:

```Gleam
import gleam/io

pub fn main() {
  file := File.open_read("textfil.txt")
  while let Some(line) = file.read_line() {
    io.print(line)
  }
}
```

Output:

```
Det här är en textfil.
Den innehåller lite exempeltext.
Det här är den tredje raden.
```

##Djupdykning
När du läser en textfil kan du också specificera en kodning, till exempel UTF-8 eller ASCII, beroende på hur filen är formaterad. Detta gör du genom att lägga till `encoding`-argumentet till `open_read()`-funktionen. Om du vill göra utföra ytterligare åtgärder på filens innehåll, som att redigera och spara det, kan du också använda `File.open_write()`-funktionen. Här är ett exempel på hur du kan läsa innehållet i en fil och lägga till ordet "Gleam" på varje rad och sedan spara ändringarna:

```Gleam
import gleam/io

pub fn main() {
  file := File.open_read("textfil.txt")
  new_content := ""

  while let Some(line) = file.read_line() {
    new_line := line ++ "gleam"
    new_content = new_content ++ new_line
  }
  
  file.close()

  file_to_write := File.open_write("new_textfil.txt")
  file_to_write.write(new_content)
  file_to_write.close()
}
```

##Se också
För mer information om filhantering i Gleam, ta en titt på följande resurser:

- [Gleam Language Reference - Fil](https://gleam.run/book/std_lib/files.html)
- [Gleam Language Guide - Läsa och skriva filer](https://gleam.run/book/tutorials/files.html)
- [Officiell dokumentation för File-modulen](https://github.com/gleam-lang/gleam_stdlib/blob/master/gleam_io/src/io/file.gleam)