---
title:                "Praca z JSON"
date:                  2024-01-19
html_title:           "Bash: Praca z JSON"
simple_title:         "Praca z JSON"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/javascript/working-with-json.md"
---

{{< edit_this_page >}}

## What & Why?
JSON (JavaScript Object Notation) to format wymiany danych. Programiści używają go ze względu na prostotę i powszechną kompatybilność z różnymi językami programowania.

## How to:
**Tworzenie obiektu JSON:**
```Javascript
let user = {
  name: "Jan",
  age: 30
};
```

**Konwersja obiektu na JSON:**
```Javascript
let json = JSON.stringify(user);
console.log(json);  // {"name":"Jan","age":30}
```

**Przetwarzanie JSON na obiekt JavaScript:**
```Javascript
let userData = '{"name":"Jan","age":30}';
let userObj = JSON.parse(userData);
console.log(userObj.name);  // Jan
```

## Deep Dive
*Historia*: JSON został zaprojektowany w ramach JavaScript, ale dziś jest niezależnym standardem (RFC 7159).

*Alternatywy*: XML był kiedyś dominującym formatem, ale JSON jest prostszy i lżejszy.

*Szczegóły implementacji*: JSON obsługuje typy danych takie jak liczby, łańcuchy, obiekty i tablice. Nieobsługiwane typy, np. `Date`, wymagają dodatkowej konwersji.

## See Also
[Dokumentacja JSON w MDN](https://developer.mozilla.org/pl/docs/Web/JavaScript/Reference/Global_Objects/JSON)
[RFC 7159 – Standard JSON](https://tools.ietf.org/html/rfc7159)
[JSON vs XML](https://www.w3schools.com/js/js_json_xml.asp)
