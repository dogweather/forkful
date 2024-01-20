---
title:                "Praca z JSON"
html_title:           "Bash: Praca z JSON"
simple_title:         "Praca z JSON"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/fish-shell/working-with-json.md"
---

{{< edit_this_page >}}

## What & Why?
Praca z JSON to manipulacja danymi w popularnym formacie tekstowym. Programiści korzystają z JSON, bo jest łatwy do czytania dla człowieka i maszyn.

## How to:
```Fish Shell
# Zainstaluj jq - narzędzie do obsługi JSON.
sudo apt install jq

# Parsuj JSON używając jq.
echo '{"name": "Jan", "age": 25}' | jq '.'
```
Output:
```json
{
  "name": "Jan",
  "age": 25
}
```
```Fish Shell
# Pobierz wartość pola 'name'.
echo '{"name": "Jan", "age": 25}' | jq '.name'
```
Output:
```json
"Jan"
```

## Deep Dive
JSON (JavaScript Object Notation) powstał z JavaScript, ale jest niezależny od języka. Alternatywą dla JSON może być XML, jednak JSON jest prostszy i szybszy w przetwarzaniu. Wpływa na to jego minimalistyczny design i czytelność. W Fish Shell używamy narzędzi trzecich jak `jq` do pracy z JSON, bo Fish ma ograniczone wbudowane wsparcie dla tego formatu.

## See Also
- [JSON Specification](https://www.json.org/json-en.html)
- [jq Tutorial](https://stedolan.github.io/jq/tutorial/)
- [Fish Shell Documentation](https://fishshell.com/docs/current/index.html)