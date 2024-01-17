---
title:                "Hämta aktuellt datum"
html_title:           "Rust: Hämta aktuellt datum"
simple_title:         "Hämta aktuellt datum"
programming_language: "Rust"
category:             "Rust"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/rust/getting-the-current-date.md"
---

{{< edit_this_page >}}

## Vad & Varför?
Att få dagens datum är en vanlig uppgift för programmerare, speciellt när man arbetar med tidsbaserade funktioner. Genom att få dagens datum kan man till exempel skapa tidstämplar för specifika händelser eller hålla koll på förändringar över tid.

## Hur man gör:
Enklaste sättet att få dagens datum i Rust är att använda standardbiblioteket DateTime. Låt oss titta på ett kodexempel:

```Rust
use std::time::SystemTime;

let now = SystemTime::now(); // Hämtar aktuell tid
let since_epoch = now.duration_since(SystemTime::UNIX_EPOCH).expect("Invali