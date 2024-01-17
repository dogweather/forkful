---
title:                "Arbeide med csv"
html_title:           "Swift: Arbeide med csv"
simple_title:         "Arbeide med csv"
programming_language: "Swift"
category:             "Swift"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/swift/working-with-csv.md"
---

{{< edit_this_page >}}

CSV gjør det enklere for programmerere å lagre og håndtere store mengder data, spesielt i tabellariske formater. Dette gjøres ved å lagre dataene i tekstfiler med komma-separerte verdier. CSV bruker lite plass og er enkelt å lese og manipulere. Det er derfor et populært format blant programmerere for lagring og utveksling av data.

## Hva & Hvorfor?

CSV er et akronym for "Comma-Separated Values", som på norsk betyr "komma-separerte verdier". Det er et standardisert filformat som brukes til å lagre og organisere data. Programmerere bruker CSV fordi det er enkelt å lese og skrive dataene, og det tar lite plass sammenlignet med andre formater.

## Slik gjør du det:

```Swift
let data = "Navn, Alder, Jobb\nPer, 25, Utvikler\nKari, 30, Designer"
let lines = data.split(separator: "\n")
// Deler opp dataen i hver linje basert på linjeskift.
for line in lines {
    let items = line.split(separator: ", ")
    // Deler opp linjen basert på komma og plasserer hver verdi i en egen array.
    print("Navn: \(items[0]) | Alder: \(items[1]) | Jobb: \(items[2])")
    // Printer ut hver verdi med riktig format og separator.
}
```

Output:
```
Navn: Per | Alder: 25 | Jobb: Utvikler
Navn: Kari | Alder: 30 | Jobb: Designer
```

## Dypdykk:

CSV-formatet ble opprinnelig utviklet for regnearkprogrammer som Excel, og har blitt brukt siden 1970-tallet. Det finnes også alternative formater som XML og JSON, men CSV er fortsatt det foretrukne valget for mange programmerere på grunn av dets enkelhet og effektivitet. Implementasjonen av CSV varierer mellom forskjellige programmeringsspråk, men de fleste har standardfunksjoner for å lese og skrive CSV-data.

## Se også:

- [Swift Guide: Lesing og skriving av CSV](https://medium.com/@khushwanttanwar/read-write-csv-file-in-swift-3f150072c0ed)
- [CSV i Nettleseren](https://www.npmjs.com/package/csv)
- [Sammenligning av XML, JSON og CSV](https://raygun.com/blog/xml-json-csv/)