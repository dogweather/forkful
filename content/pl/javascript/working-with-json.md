---
title:                "Praca z formatem json"
html_title:           "Javascript: Praca z formatem json"
simple_title:         "Praca z formatem json"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/javascript/working-with-json.md"
---

{{< edit_this_page >}}

## Co to jest i po co to robić?

JSON to format pliku danych, który jest używany przez programistów do przechowywania i przesyłania informacji w wielu formatach. Jest to kompaktowy i czytelny dla ludzi sposób przechowywania danych, co czyni go popularnym w świecie programowania.

## Jak to zrobić:

```Javascript
// Przykładowy obiekt JSON
let osoba = {
  "imię": "Anna",
  "nazwisko": "Kowalska",
  "wiek": 30,
  "hobby": ["czytanie", "sport"]
}

// Konwertowanie na format JSON
let json = JSON.stringify(osoba);

// Wyświetlenie danych JSON w konsoli
console.log(json);

// Wynik: {"imię": "Anna", "nazwisko": "Kowalska", "wiek": 30, "hobby": ["czytanie", "sport"]}
```

## Głębsze zagadnienia:

JSON został stworzony w 2001 roku przez Douga Crockforda i szybko stał się popularnym formatem w świecie internetu. Alternatywą dla JSON jest format XML, jednak JSON jest zdecydowanie łatwiejszy do czytania i pisania. W Javascript dostępne są funkcje: `JSON.stringify()` do konwertowania obiektów do formatu JSON oraz `JSON.parse()` do przetwarzania danych JSON z powrotem na format obiektów.

## Zobacz też:

Więcej informacji o JSON znajdziesz na [oficjalnej stronie dokumentacji](https://www.json.org/json-pl.html). Możesz również skorzystać z [JSON Editor Online](http://jsoneditoronline.org/) aby lepiej zrozumieć strukturę danych JSON.