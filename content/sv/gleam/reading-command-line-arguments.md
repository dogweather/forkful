---
title:                "Läsa kommandoradsargument"
html_title:           "Gleam: Läsa kommandoradsargument"
simple_title:         "Läsa kommandoradsargument"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/gleam/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## Varför

Att kunna läsa in kommandoradsargument är en viktig färdighet för alla som programmerar i Gleam. Det tillåter dig att anpassa ditt program medan det körs och öppnar möjligheter för interaktion mellan användare och program.

## Hur man gör

Att läsa in kommandoradsargument i Gleam är en enkel process. Du behöver bara importera modulen `command_line` och sedan anropa funktionen `parse` för att få en lista med argumenten.

```Gleam
external type List(a)

external type String {
    Format(a) -> String
}

// Importera modulen command_line
import gleam/command_line

// Anropa funktionen parse för att få en lista med argumenten
let arguments = command_line.parse()

// Skriv ut listan med argumenten
List.map(args, String.Format) |> Debug.log
```

### Exempel

Om du till exempel kör detta program med kommandoradsargumentet `name=John` kommer listan med argumenten att se ut som `["name=John"]`. Om du vill separera argumentet och värdet kan du använda funktionen `String.split` som visas i exemplet nedan.

```Gleam
let arguments = command_line.parse()

let [key, value] = case String.split("=", List.head(arguments)) {
    Some(result) -> result
    None -> ["", ""]
}

String.Format("Argumentet `{}` har värdet `{}`", key, value)
|> Debug.log
```

#### Körkommando

```bash
gleam run main.gleam -- name=John
```

#### Output

```
Argumentet `name` har värdet `John`
```

## Djupdykning

Det finns möjligheter att anpassa hur du läser in kommandoradsargument genom att använda funktionerna `parse_with` och `parse_with_help` från modulen `command_line`. Dessa funktioner tar en konfigurationssträng som parameter för att specificera regler för hur argumenten ska tolkas.

### Konfigurationssträngen

Konfigurationssträngen består av flera delar som separeras med ett mellanslag. Den första delen är argumentnamnet som ska följas av en listformatering för att specificera hur argumentet ska tolkas.

#### Listformaten

- `named_flag` - tolkar argumentet som en flagga med en tillhörande boolean som värde
- `named_argument` - tolkar argumentet som en sträng med en tillhörande värdestring
- `repeated_named_argument` - tolkar argumentet som en lista med flera värden
- `program_argument` - tolkar argumentet som en sträng utan ett fördefinierat namn

#### Exempel

Konfigurationssträngen `name:named_argument ` skulle tolka ett argument med namnet `name` som en sträng med ett tillhörande värde.

## Se även

- [Officiell dokumentation för command_line-modulen](https://gleam.run/modules/command_line)
- [GitHub-repositorium för Gleam](https://github.com/gleam-lang/gleam)