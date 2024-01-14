---
title:    "Gleam: Kontrollera om en mapp finns"
keywords: ["Gleam"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/sv/gleam/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

# Varför

Att kolla om en mapp existerar är en vanlig uppgift för programmerare, särskilt när man arbetar med filsystemet i sina projekt. Det kan också hjälpa till att förhindra fel och krockar i koden. I denna bloggpost kommer vi att utforska hur du kan använda Gleam för att enkelt kolla om en mapp finns eller inte.

# Hur man gör det

För att kolla om en mapp existerar i Gleam, kan vi använda funktionen `Dir.exists()` som finns i `gleam/os` biblioteket. Den tar emot en `String` som representerar mappen som ska kollas och returnerar en `Bool` som indikerar om mappen existerar eller inte. Här är ett exempel på hur vi kan använda den funktionen:

```Gleam
import gleam/os

fn check_exists() {
  let mapp = "./mina-filer/"
  let finns_mapp = gleam/os.Dir.exists(mapp)
  Gleam.IO.print("Mappen finns: ")
  Gleam.IO.print(finns_mapp)
}

check_exists()
```

I det här exemplet kollar vi om mappen "mina-filer" existerar och skriver sedan ut resultatet. Om mappen finns kommer "Mappen finns: true" att skrivas ut, annars kommer "Mappen finns: false" att skrivas ut.

# Djupdykning

I Gleam, som i många andra programmeringsspråk, är mappar faktiskt bara en typ av fil. Detta betyder att `Dir.exists()` funktionen även kan användas för att kolla om en fil existerar. Det är också viktigt att notera att funktionen inte utvärderar om en fil är tillgänglig eller inte, bara om den existerar eller inte. Om du behöver kolla om en fil är tillgänglig kan du använda funktionen `File.exists()` istället.

# Se även

- Officiell Gleam dokumentation: https://gleam.run/
- Gleam GitHub repository: https://github.com/gleam-lang/gleam