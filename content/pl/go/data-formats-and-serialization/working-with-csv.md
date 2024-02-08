---
title:                "Praca z plikami CSV"
date:                  2024-02-03T18:11:51.342769-07:00
model:                 gpt-4-0125-preview
simple_title:         "Praca z plikami CSV"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/go/working-with-csv.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Co i dlaczego?

Format wartości rozdzielanych przecinkami (CSV) jest wszechobecny w wymianie danych ze względu na swoją prostotę i łatwość integracji z większością języków programowania, w tym Go. Programiści często pracują z plikami CSV w celu migracji danych, generowania raportów lub analizy danych, co czyni zrozumienie manipulacji CSV kluczowym elementem w zestawie narzędzi do tworzenia oprogramowania.

## Jak to zrobić:

Praca z plikami CSV w Go jest prosta, dzięki jego standardowej bibliotece, `encoding/csv`. Poniżej znajduje się wprowadzenie do czytania i pisania plików CSV.

### Czytanie pliku CSV

Aby odczytać plik CSV, najpierw otwierasz plik za pomocą `os.Open`, a następnie tworzysz nowego czytelnika CSV za pomocą `csv.NewReader`.

```go
package main

import (
    "encoding/csv"
    "fmt"
    "os"
)

func main() {
    file, err := os.Open("data.csv")
    if err != nil {
        panic(err)
    }
    defer file.Close()

    reader := csv.NewReader(file)
    records, err := reader.ReadAll()
    if err != nil {
        panic(err)
    }

    for _, record := range records {
        fmt.Println(record)
    }
}
```

Fragment kodu odczyta wszystkie rekordy z `data.csv` i wydrukuje je. Każdy rekord to ciąg pól.

### Pisanie do pliku CSV

Do pisania używasz `csv.NewWriter` i `writer.WriteAll` lub `writer.Write` do zapisywania wielu lub pojedynczych rekordów CSV odpowiednio.

```go
package main

import (
    "encoding/csv"
    "os"
)

func main() {
    file, err := os.Create("output.csv")
    if err != nil {
        panic(err)
    }
    defer file.Close()

    writer := csv.NewWriter(file)
    defer writer.Flush()

    records := [][]string{
        {"Name", "Age", "City"},
        {"John Doe", "30", "New York"},
        {"Jane Doe", "27", "Los Angeles"},
    }

    if err := writer.WriteAll(records); err != nil {
        panic(err)
    }
}
```

To utworzy plik o nazwie `output.csv` z podanymi rekordami. Zawsze pamiętaj, aby przepłukać buffer pisarza, aby upewnić się, że wszystkie buforowane dane zostały zapisane do pliku.

## Głębsze zagłębienie

Pakiet Go `encoding/csv` zapewnia solidne wsparcie dla czytania i pisania plików CSV, ale jest zaprojektowany z myślą o prostocie, co oznacza, że nie radzi sobie z bardziej złożonymi scenariuszami, takimi jak automatyczne wykrywanie delimiterów, radzenie sobie z cytowaniami czy wbudowanymi łamaniem linii w polach bez ręcznej obsługi.

Historycznie, obsługa CSV w językach programowania często była uciążliwa z powodu tych złożoności, ale standardowa biblioteka Go abstrahuje wiele z tych problemów, pozwalając programistom pracować z danymi CSV z względną łatwością. Jednakże, do bardziej złożonej manipulacji CSV, mogą być konieczne biblioteki stron trzecich, takie jak `gocsv` lub ręczne przetwarzanie.

Godnym uwagi aspektem pakietu `csv` Go jest jego wsparcie dla określania niestandardowych przecinków (delimiterów), co pozwala na bezproblemową pracę z wariantami plików CSV, takimi jak wartości rozdzielane tabulacją (TSV). Jednakże, podczas pracy z wysoce nieregularnymi lub niestandardowymi plikami CSV, programiści Go mogą znajdować się w potrzebie rozszerzenia istniejących implementacji czytelnika lub pisarza csv.

Chociaż możliwości obsługi CSV w Go są solidne do celów ogólnych, dla aplikacji wymagających intensywnej manipulacji danymi, takich jak nauka o danych czy złożone zadania transformacji danych, programiści mogą rozważyć dedykowane pakiety przetwarzania danych lub nawet inne języki lepiej przystosowane do tych zadań, takie jak Python z jego biblioteką `pandas`. Niemniej jednak, do prostych operacji odczytu i zapisu CSV, standardowa biblioteka Go wyróżnia się ze względu na swoją wydajność i prostotę.
