---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:21:43.312661-07:00
description: "Praca z JSON w Bash polega na parsowaniu, wydobywaniu i manipulowaniu\
  \ danymi JSON bezpo\u015Brednio z linii polece\u0144. Programi\u015Bci cz\u0119\
  sto robi\u0105 to, aby p\u0142ynnie\u2026"
lastmod: '2024-03-11T00:14:08.798887-06:00'
model: gpt-4-0125-preview
summary: "Praca z JSON w Bash polega na parsowaniu, wydobywaniu i manipulowaniu danymi\
  \ JSON bezpo\u015Brednio z linii polece\u0144. Programi\u015Bci cz\u0119sto robi\u0105\
  \ to, aby p\u0142ynnie\u2026"
title: Praca z JSON
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
