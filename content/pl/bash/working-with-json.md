---
title:                "Praca z JSON"
date:                  2024-01-19
html_title:           "Bash: Praca z JSON"
simple_title:         "Praca z JSON"

category:             "Bash"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/bash/working-with-json.md"
---

{{< edit_this_page >}}

## Co i dlaczego?
Pracujemy z JSON, bo to standard wymiany danych. Używany wszędzie – od API po konfiguracje, część naszego codziennego kodzenia.

## Jak to zrobić:
```Bash
# Parsowanie JSON za pomocą jq
echo '{"user": "janek", "likes": ["bash", "json"]}' | jq '.'

# Extrahowanie pola
echo '{"user": "janek", "likes": ["bash", "json"]}' | jq '.user'

# Dodawanie elementów do tablicy JSON
echo '{"user": "janek", "likes": ["bash"]}' | jq '.likes += ["json"]'
```

Output:
```Bash
{
  "user": "janek",
  "likes": [
    "bash",
    "json"
  ]
}
"janek"
{
  "user": "janek",
  "likes": [
    "bash",
    "json"
  ]
}
```

## Deep Dive
JSON, czyli JavaScript Object Notation, wziął się z JavaScriptu, ale stał się językiem niezależnym. Alternatywami są XML, YAML, ale JSON wygrywa prostotą. W Bash używamy `jq` do manipulacji JSON – lekki, elastyczny, można instalować jako standardowy pakiet.

## See Also
- Dokumentacja `jq`: https://stedolan.github.io/jq/manual/
- Przykłady `jq`: https://jqplay.org/
- JSON specyfikacja: https://www.json.org/json-pl.html
