---
title:                "Praca z plikami csv"
html_title:           "Swift: Praca z plikami csv"
simple_title:         "Praca z plikami csv"
programming_language: "Swift"
category:             "Swift"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/swift/working-with-csv.md"
---

{{< edit_this_page >}}

## Co to jest CSV i dlaczego programiści z niego korzystają?

CSV to format przechowywania danych w postaci tabeli, gdzie każda linia reprezentuje pojedynczy rekord, a kolumny są oddzielone przecinkami. Jest to powszechnie wykorzystywany przez programistów sposób przechowywania i udostępniania informacji. Dobrym przykładem wykorzystania CSV jest przechowywanie i przetwarzanie dużych ilości danych, takich jak listy klientów czy wyniki badań.

## Jak to zrobić:

Aby pracować z danymi w formacie CSV w Swift, możesz wykorzystać wbudowane funkcje języka, takie jak ```contentsOfCSVFile``` lub ```parseCSV```. Przykładowo, aby odczytać plik CSV z danymi klientów, można użyć poniższego kodu:

```Swift 
let fileURL = //insert file URL
do {
    let contents = try String(contentsOf: fileURL)
    let rows = contents.trimmingCharacters(in: .whitespacesAndNewlines).components(separatedBy: .newlines)
    for row in rows {
        let columns = row.components(separatedBy: ",")
        //manipulate data
    }
} catch {
    //handle error
}
```

Jeśli natomiast chcesz stworzyć plik CSV z danymi, możesz użyć funkcji ```joined(separator: ",")```, która pozwoli Ci formatować dane w odpowiedni sposób.

## Głębszy wgląd:

CSV został stworzony w 1972 roku i od tamtej pory jest szeroko wykorzystywany przez różne programy i języki programowania. Alternatywami dla tego formatu są między innymi Excel czy JSON, jednak CSV jest często wybierany ze względu na swoją prostotę i wsparcie przez wiele aplikacji.

Podczas pracy z CSV możesz mieć do czynienia z różnymi typami danych, dlatego ważne jest dokładne zapoznanie się z dokumentacją i dostosowanie swojego kodu do potrzeb. Pamiętaj także o odpowiednim formatowaniu danych, aby uniknąć błędów podczas odczytu pliku.

## Zobacz także:

Jeśli chcesz dowiedzieć się więcej o pracy z CSV w języku Swift, polecam zapoznać się z dokumentacją oficjalnej biblioteki SwiftCSV. Możesz także poszukać tutoriali i artykułów online, które pomogą Ci lepiej zrozumieć ten format danych.