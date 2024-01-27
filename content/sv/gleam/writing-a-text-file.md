---
title:                "Skriva en textfil"
date:                  2024-01-19
html_title:           "Arduino: Skriva en textfil"
simple_title:         "Skriva en textfil"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/gleam/writing-a-text-file.md"
---

{{< edit_this_page >}}

## Vad & Varför?
Att skriva en textfil innebär att spara strängar av tecken till en fil på hårddisken. Programmerare gör detta för att bevara data, som konfigurationer eller användargenererade innehåll, över tid.

## Hur till:
```gleam
// Import the needed module
import gleam/io

pub fn write_to_file() {
  // The content you want to write
  let content = "Hej! Det här är en textfil skriven med Gleam."

  // Write to a file, replace 'my_text_file.txt' with your file path
  try result = io.write_file("my_text_file.txt", content)

  // Check if writing was successful
  case result {
    Ok(_) -> io.print("Fil skriven framgångsrikt!\n")
    Error(err) -> io.print("Ett fel uppstod: " <> io.error_to_string(err) <> "\n")
  }
}

// Call the function
pub fn main() {
  write_to_file()
}
```
Output:
```
Fil skriven framgångsrikt!
```

## Djupdykning
Textfilsinlagring har använts sedan tidig datoråldern och är fortfarande en grundläggande del av programmering. Alternative metoder inkluderar databaser eller molnbaserade tjänster. Skrivprocessen i Gleam hanteras via inbyggda funktioner som `io.write_file`, som fungerar genom att kommunicera med operativsystemet för att utföra filskrivoperationer.

## Se också
- [Gleam's GitHub Repository](https://github.com/gleam-lang/gleam)
- [Gleam Language Website](https://gleam.run/)
