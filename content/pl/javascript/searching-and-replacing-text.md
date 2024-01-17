---
title:                "Wyszukiwanie i zamiana tekstu"
html_title:           "Javascript: Wyszukiwanie i zamiana tekstu"
simple_title:         "Wyszukiwanie i zamiana tekstu"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/javascript/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## Co to jest i dlaczego to robić?

W programowaniu często występuje potrzeba zmiany tekstu wewnątrz naszego kodu. Na przykład, chcielibyśmy zmienić wszystkie wystąpienia słowa "kot" na "pies", lub usunąć określone fragmenty z tekstu. Do tego służy tzw. wyszukiwanie i zastępowanie tekstu, czyli proces znajdowania i zmiany konkretnych fragmentów tekstu w danym pliku lub programie. Programiści korzystają z tej funkcji, aby ułatwić sobie i przyspieszyć pracę nad swoim kodem.

## Jak to zrobić:

### Wyszukiwanie tekstu:
W JavaScript możemy wyszukiwać tekst przy użyciu wbudowanej funkcji `indexOf()` lub `lastIndexOf()`. Obie funkcje zwracają indeks pierwszego lub ostatniego znalezionego wystąpienia szukanego słowa. Możemy też użyć wyrażeń regularnych, co pozwoli na bardziej zaawansowane i precyzyjne wyszukiwanie.

#### Przykład:
```javascript
let text = "Kotek to mały łowca myszy";
let index = text.indexOf("kot"); // zwróci 0
// ponieważ pierwsze wystąpienie słowa "kot" znajduje się na początku tekstu
```
```javascript
let text = "Kotek to mały łowca myszy";
let lastIndex = text.lastIndexOf("kot"); // zwróci 18
// ponieważ ostatnie wystąpienie słowa "kot" znajduje się na pozycji 18
```

### Zastępowanie tekstu:
Aby zastąpić dany fragment tekstu, możemy użyć funkcji `replace()` wraz z wyrażeniami regularnymi. Ta funkcja zastąpi pierwsze wystąpienie wyszukanego wyrażenia danym tekstem lub innym wyrażeniem.

#### Przykład:
```javascript
let text = "Kotek to mały łowca myszy";
let newText = text.replace("kot", "pies"); // zwróci "Piesek to mały łowca myszy"
```
```javascript
let text = "Kotek to mały łowca myszy";
let newText = text.replace(/kot/g, "pies"); // zwróci "Piesek to mały łowca myszy"
// dzięki użyciu flagi 'g' zastąpione zostaną wszystkie wystąpienia
```

## Głębszy zanurzenie:

### Kontekst historyczny:
Wyszukiwanie i zastępowanie tekstu jest nieodłączną częścią programowania już od początków tej dziedziny. Pierwsze narzędzia do przetwarzania tekstu pojawiły się w latach 50. XX wieku, a od tego czasu stały się nieodzownym elementem każdego języka programowania.

### Inne metody:
W zależności od języka programowania, istnieje wiele różnych metod i funkcji do wyszukiwania i zastępowania tekstu. JavaScript oferuje wbudowane funkcje, ale w niektórych przypadkach może być też przydatne użycie narzędzi zewnętrznych.

### Szczegóły implementacji:
W celu wykonania wyszukiwania i zastępowania tekstu w JavaScript, wykorzystywane są wyrażenia regularne, czyli specjalne wzorce służące do dopasowania i manipulacji tekstami. Ważne jest także użycie odpowiednich funkcji, aby dokładnie określić czego szukamy i jakie zmiany chcemy wprowadzić.

## Zobacz także:
- [Metody String w JavaScript](https://developer.mozilla.org/pl/docs/Web/JavaScript/Referencje/Obiekty/String)
- [Wyrażenia regularne w JavaScript](https://developer.mozilla.org/pl/docs/Web/JavaScript/Guide/Regular_Expressions)