---
title:                "Analizowanie html"
html_title:           "Arduino: Analizowanie html"
simple_title:         "Analizowanie html"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/arduino/parsing-html.md"
---

{{< edit_this_page >}}

## Dlaczego

Kody HTML są często używane do tworzenia stron internetowych, jednak czasami może być potrzebne ich parsowanie w celu odczytania i wykorzystania zawartości. W tym artykule dowiesz się, jak za pomocą Arduino możesz wykonywać to zadanie.

## Jak to zrobić

Parsowanie HTML za pomocą Arduino jest prostsze, niż mogłoby się wydawać. Wystarczy użyć gotowej biblioteki o nazwie "HTML Parser", którą możesz pobrać z oficjalnego repozytorium Arduino lub zainstalować wtyczkę do swojego środowiska programistycznego.

Przykładowy kod wraz z wyjściem wyglądałby następująco:

```
#include <HTMLParser.h> // Importowanie biblioteki

HTMLParser parser; // Inicjalizacja obiektu parsera HTML

void setup() {
  Serial.begin(9600); // Inicjalizacja komunikacji z komputerem
  parser.begin(Serial); // Uruchomienie parsera na porcie Serial
}

void loop() {
  if (Serial.available() > 0) { // Sprawdzenie, czy są dostępne dane
    parser.parse(Serial); // Parsowanie danych z portu Serial
    if (parser.isTag()) { // Sprawdzenie, czy to jest tag HTML
      parser.printTag(); // Wyświetlenie tagu
    } else if (parser.isText()) { // Sprawdzenie, czy to jest tekst HTML
      parser.printTag(); // Wyświetlenie tekstu
    }
  }
}
```

Przykładowe wyjście:

```
<html> // Tag
<head> // Tag
<title> // Tag
Parsowanie HTML za pomocą Arduino // Tekst
</title> // Tag
</head> // Tag
<body> // Tag
<h1> // Tag
Witaj na mojej stronie internetowej! // Tekst
</h1> // Tag
</body> // Tag
</html> // Tag
```

W powyższym przykładzie, za pomocą metody `isTag()` sprawdzamy, czy otrzymany znak jest tagiem, a następnie wyświetlamy go za pomocą metody `printTag()`. Podobnie postępujemy z tekstem, wykorzystując odpowiednie metody.

## Głębszy zanurzenie

Biblioteka "HTML Parser" oferuje także wiele innych metod, dzięki którym możesz dokładnie kontrolować proces parsowania HTML. Możesz na przykład ustawić, żeby parser ignorował niektóre tagi lub pobierał tylko zawartość konkretnych tagów. Więcej informacji na ten temat znajdziesz w oficjalnej dokumentacji biblioteki.

## Zobacz też

- [Oficjalna dokumentacja biblioteki "HTML Parser"](https://arduinojson.org/doc/)
- [Oficjalna strona Arduino](https://www.arduino.cc/)
- [Artykuł o podstawach programowania Arduino](https://www.nettiny.net/pl/artykul/2-Jak-zaczac-programowac-Arduino)