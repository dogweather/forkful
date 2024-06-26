---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:21:43.312661-07:00
description: "Jak to zrobi\u0107: Bash sam w sobie nie posiada wbudowanych mo\u017C\
  liwo\u015Bci parsowania JSON, ale `jq` jest pot\u0119\u017Cnym narz\u0119dziem do\
  \ przetwarzania JSON z linii\u2026"
lastmod: '2024-03-13T22:44:35.606784-06:00'
model: gpt-4-0125-preview
summary: "Bash sam w sobie nie posiada wbudowanych mo\u017Cliwo\u015Bci parsowania\
  \ JSON, ale `jq` jest pot\u0119\u017Cnym narz\u0119dziem do przetwarzania JSON z\
  \ linii polece\u0144, kt\xF3re wype\u0142nia t\u0119 luk\u0119."
title: Praca z JSON
weight: 38
---

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
