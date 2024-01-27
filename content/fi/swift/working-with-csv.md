---
title:                "CSV-tiedostojen käsittely"
date:                  2024-01-19
html_title:           "Bash: CSV-tiedostojen käsittely"
simple_title:         "CSV-tiedostojen käsittely"
programming_language: "Swift"
category:             "Swift"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/swift/working-with-csv.md"
---

{{< edit_this_page >}}

## Mikä & Miksi?
CSV (Comma-Separated Values) on tiedostomuoto, joka tallentaa taulukollista tietoa, kuten Excelissä. Ohjelmoijat käyttävät CSV:tä, koska se on yksinkertainen, laajasti tuettu ja helppo vaihtaa eri ohjelmien välillä.

## How to:
Swiftissä CSV:n käsittely onnistuu helposti. Tässä perusesimerkki:

```Swift
import Foundation

// Oletetaan, että `csvString` sisältää CSV-muotoista dataa
let csvString = """
name,age,city
John Doe,29,Helsinki
Jane Smith,34,Espoo
"""

// Funktio CSV-datan muuntamiseen arrayksi dictionaryjä
func parseCSV(contents: String) -> [[String: String]] {
    var result: [[String: String]] = []
    let rows = contents.components(separatedBy: "\n")

    // Otetaan otsikkorivi
    guard let headers = rows.first?.components(separatedBy: ",") else { return [] }
    for row in rows.dropFirst() {
        let columns = row.components(separatedBy: ",")
        let entry = Dictionary(zip(headers, columns), uniquingKeysWith: { first, _ in first })
        result.append(entry)
    }
    
    return result
}

// CSV-datan käsittely ja tulostaminen
let parsedData = parseCSV(contents: csvString)
for user in parsedData {
    print("Name: \(user["name"] ?? ""), Age: \(user["age"] ?? ""), City: \(user["city"] ?? "")")
}
```

Sample output:
```
Name: John Doe, Age: 29, City: Helsinki
Name: Jane Smith, Age: 34, City: Espoo
```

## Deep Dive
CSV on syntynyt 1970-luvulla ja on siitä lähtien toiminut yksinkertaisena tapana tallentaa taulukollista tietoa. Vaikka se ei tue tietotyyppejä eikä monitahoista rakennetta, sen yksinkertaisuus tekee siitä ihanteellisen suurien datamäärien nopeaan siirtämiseen. JSON ja XML ovat monipuolisempia vaihtoehtoja, mutta eivät yhtä kevyitä. CSV:n käsittely Swiftissä edellyttää tiedon erottelua ja muuntamista käsiteltävään muotoon, mutta se on suoraviivaista, kuten koodiesimerkissä nähty.

## See Also
Lisätietoja ja resursseja Swiftin ja CSV:n käsittelystä:

- Swift Standard Library: https://developer.apple.com/documentation/swift
- CSV:n erottimien käsittely: https://en.wikipedia.org/wiki/Comma-separated_values
- Swiftin tiedostonkäsittely: https://developer.apple.com/documentation/foundation/filemanager

Tarkempaa tietoa saat tutustumalla Swiftin dokumentaatioon ja CSV-formaatin standardiin.
