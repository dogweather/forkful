---
title:                "Praca z json"
html_title:           "Bash: Praca z json"
simple_title:         "Praca z json"
programming_language: "Bash"
category:             "Bash"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/bash/working-with-json.md"
---

{{< edit_this_page >}}

## Dlaczego

Praca z formatem JSON jest nieodłączną częścią wielu zadań programistycznych w Bashu. Wiedza na temat tego formatu jest niezbędna do skutecznego przetwarzania danych w różnych projektach.

## Jak to zrobić

Podstawowym narzędziem w celu pracy z JSON w Bashu jest polecona komenda `jq`. Przykładowe użycie wygląda następująco:

```Bash
response='{"name": "John", "age": 26}'
echo "$response" | jq '.name'

```

Powyższy przykład zwróci wartość `"John"` dla klucza `"name"`. Inne przydatne funkcje `jq` to `select` i `map`, które pozwalają filtrować odpowiedzi w bardziej zaawansowany sposób. Przykładowe wykorzystanie:

```Bash
response='[{"name": "John", "age": 26}, {"name": "Mary", "age": 30}]'
echo "$response" | jq 'map(select(.age >= 30))'

```

Wynikiem będzie tablica z jednym elementem o wartości `{ "name": "Mary", "age": 30 }`.

## Deep Dive

Format JSON jest powszechnie stosowany do przechowywania i przesyłania danych w aplikacjach webowych i mobilnych. Jest to zapisywany w postaci tekstu, co czyni go łatwym do odczytania przez ludzi i łatwym do przetwarzania przez komputery.

Komenda `jq` jest również wykorzystywana do wstępnie przetwarzania danych zanim są przekazane do innych narzędzi. Dzięki temu można szybko i łatwo wyodrębnić potrzebne informacje z dużych i złożonych struktur danych.

## Zobacz również

- Dokumentacja oficjalna `jq`: https://stedolan.github.io/jq/
- Przetwarzanie JSON w Bashu z użyciem `jq`: https://www.baeldung.com/linux/jq-json-processing-bash
- Inne narzędzia pomocne w pracy z JSON w Bashu: https://www.computerhope.com/unix/jq.htm