---
title:                "Gleam: Sammanslagning av strängar"
programming_language: "Gleam"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/gleam/concatenating-strings.md"
---

{{< edit_this_page >}}

## Varför

Att sammanslå strängar är en vanlig aktivitet inom programmering som gör det möjligt att skapa mer dynamiska och anpassningsbara textsträngar. Det är ett viktigt koncept att förstå för att kunna bygga kraftfulla program.

## Hur man gör

För att sammanslå strängar i Gleam behöver du använda funktionen ```str.concat()```, som tar två strängar som argument och returnerar dem sammanslagna som en enda sträng. Här är ett exempel på hur du kan använda funktionen:

```Gleam
let förnamn = "Anna"
let efternamn = "Svensson"
let namn = str.concat(förnamn, efternamn)
```

Output: "Anna Svensson"

## Djupdykning

Förutom att sammanslå två strängar, kan du också använda ```str.concat()``` för att kombinera flera strängar samtidigt. Detta görs genom att lägga till fler strängargument till funktionen. Till exempel:

```Gleam
let förnamn = "Anna"
let mellannamn = "Maria"
let efternamn = "Svensson"
let namn = str.concat(förnamn, mellannamn, efternamn)
```

Output: "Anna Maria Svensson"

En annan användbar funktion för att sammanslå strängar är ```str.join()```, som tar en lista av strängar och ett skiljetecken som argument. Den returnerar en sammanslådd sträng av listans element med skiljetecknet mellan dem. Exempel:

```Gleam
let månader = ["Januari", "Februari", "Mars", "April"]
let resultat = str.join(månader, ", ")
```

Output: "Januari, Februari, Mars, April"

## Se även

- [Officiell Gleam dokumentation för strängmanipulation] (https://gleam.run/documentation/stdlib/string/)
- [Gleam strängmanipulationsmoduler] (https://github.com/gleam-lang/unicode)