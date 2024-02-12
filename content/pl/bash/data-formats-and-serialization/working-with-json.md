---
title:                "Praca z JSON"
aliases:
- /pl/bash/working-with-json.md
date:                  2024-02-03T19:21:43.312661-07:00
model:                 gpt-4-0125-preview
simple_title:         "Praca z JSON"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/bash/working-with-json.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Co i dlaczego?
Praca z JSON w Bash polega na parsowaniu, wydobywaniu i manipulowaniu danymi JSON bezpośrednio z linii poleceń. Programiści często robią to, aby płynnie integrować skrypty powłoki z interfejsami API sieciowymi i nowoczesnymi formatami wymiany danych, co sprawia, że skryptowanie w Bashu staje się bardziej potężne i aktualne w ekosystemie zdominowanym przez JSON.

## Jak to zrobić:
Bash sam w sobie nie posiada wbudowanych możliwości parsowania JSON, ale `jq` jest potężnym narzędziem do przetwarzania JSON z linii poleceń, które wypełnia tę lukę. Oto jak go używać:

**Czytanie pliku JSON:**

Przykładowy `data.json`:
```json
{
  "name": "Jane Doe",
  "email": "jane@example.com",
  "location": {
    "city": "New York",
    "country": "USA"
  }
}
```

Aby odczytać i wydobyć nazwisko z pliku JSON:
```bash
jq '.name' data.json
```
Wyjście:
```
"Jane Doe"
```

**Modyfikowanie danych JSON:**

Aby zaktualizować miasto na "Los Angeles" i zapisać z powrotem do pliku:
```bash
jq '.location.city = "Los Angeles"' data.json > temp.json && mv temp.json data.json
```

**Parsowanie JSON z zmiennej:**

Jeśli masz JSON w zmiennej Bash, `jq` może go także przetworzyć:
```bash
json_string='{"name": "John Doe", "email": "john@example.com"}'
echo $json_string | jq '.name'
```
Wyjście:
```
"John Doe"
```

**Praca z tablicami:**

Mając tablicę elementów w JSON:
```json
{
  "items": ["apple", "banana", "cherry"]
}
```

Aby wydobyć drugi element (indeksowanie zaczyna się od 0):
```bash
jq '.items[1]' data.json
```
Wyjście:
```
"banana"
```

Dla bardziej zaawansowanych operacji i filtrowania, `jq` posiada obszerną instrukcję obsługi i tutoriale dostępne online, co czyni go wszechstronnym narzędziem dla wszystkich twoich potrzeb związanych z Bash/JSON.
