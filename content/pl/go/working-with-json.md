---
title:                "Praca z formatem json"
html_title:           "Go: Praca z formatem json"
simple_title:         "Praca z formatem json"
programming_language: "Go"
category:             "Go"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/go/working-with-json.md"
---

{{< edit_this_page >}}

## Co i dlaczego?
JSON to język znaczników, który jest powszechnie wykorzystywany przez programistów do przechowywania i przesyłania danych między aplikacjami. Jest to kompaktowy i czytelny dla ludzi format, co czyni go idealnym do komunikacji między aplikacjami. Programiści często wykorzystują JSON, ponieważ jest on wygodny w użyciu i prosty do zrozumienia.

## Jak to zrobić: 
```Go
import (
    "encoding/json"
    "fmt"
)
```

Kod powyżej importuje pakiet "encoding/json", który zawiera funkcje do kodowania i dekodowania danych do formatu JSON. Następnie przy użyciu funkcji json.Marshal() możemy przekonwertować dane do formatu JSON, a funkcja json.Unmarshal() pozwala na dekodowanie danych z formatu JSON. Poniżej znajdują się przykłady wykorzystania tych funkcji:

```Go
data := map[string]string{"name": "John", "age": "30"}

// Kodowanie danych do formatu JSON
encoded, err := json.Marshal(data)
if err != nil {
    fmt.Println(err)
}
// Wyświetlenie wyniku
fmt.Println(string(encoded))

// Dekodowanie danych z formatu JSON do typu map[string]string
var decoded map[string]string
err := json.Unmarshal(encoded, &decoded)
if err != nil {
    fmt.Println(err)
}
// Wyświetlenie wyniku
fmt.Println(decoded["name"])
fmt.Println(decoded["age"])
```

Powyższy kod wyświetli następujący wynik:

```
{"name":"John","age":"30"}
John
30
```

## Głębsza analiza:
### Kontekst historyczny:
JSON został stworzony w 2001 roku i jest uważany za powszechny format wymiany danych w dzisiejszych czasach. Powstał jako alternatywa dla formatu XML, który często jest uważany za nieefektywny i zbyt złożony.

### Alternatywy:
Alternatywą dla JSON może być format XML lub YAML. Jednak JSON cieszy się popularnością ze względu na swoją prostotę i czytelność.

### Szczegóły implementacji:
Pakiet "encoding/json" w Go został stworzony w celu zapewnienia wygodnego sposobu kodowania i dekodowania danych w formacie JSON. Pakiet ten wykorzystuje refleksję w celu automatycznego dopasowania typów danych, co ułatwia pracę z danymi w różnych formatach.

## Zobacz także:
W przypadku dalszego zainteresowania tematem, warto zapoznać się z następującymi źródłami:

- Dokumentacja pakietu "encoding/json" w Go: https://golang.org/pkg/encoding/json/
- Dlaczego warto wybrać format JSON?: https://www.json.org/
- Porównanie formatów JSON, XML i YAML: https://danielmiessler.com/study/json-xml-yaml-comparison/