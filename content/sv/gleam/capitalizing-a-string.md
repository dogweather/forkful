---
title:    "Gleam: Att Kapsla en Sträng"
keywords: ["Gleam"]
---

{{< edit_this_page >}}

## Varför
Att kapitalisera en sträng är en vanlig uppgift inom programmering och kan vara användbart för att göra text mer estetiskt tilltalande eller för att jämföra strängar på ett korrekt sätt.

## Så här gör du
För att kapitalisera en sträng i Gleam kan du använda funktionen `String.capitalize` tillsammans med den sträng du vill manipulera.

```Gleam
let name = "lena"
let capitalizedName = String.capitalize(name)

IO.print(capitalizedName)
```

Output: "Lena"

Du kan också använda `String.to_upper` för att helt enkelt göra en sträng helt i stora bokstäver.

```Gleam
let text = "hello world"
let uppercaseText = String.to_upper(text)

IO.print(uppercaseText)
```

Output: "HELLO WORLD"

## Djupdykning
Vid kapitalisering av en sträng, är det viktigt att notera att det bara är det första tecknet som blir stort. Om du behöver göra om hela strängen till små bokstäver eller göra alla tecken stora, måste du använda andra funktioner.

## Se också
- Gleams dokumentation om strängar: https://gleam.run/book/introduction.html#strings
- En guide till grundläggande strängoperationer i Gleam: https://medium.com/@foojobs99/string-manipulation-in-gleam-1c1b36c0687a