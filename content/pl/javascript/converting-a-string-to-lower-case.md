---
title:                "Javascript: Konwertowanie łańcucha znaków na małe litery"
programming_language: "Javascript"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/javascript/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## Dlaczego

W tym poście dowiesz się, dlaczego konwertowanie stringa na małe litery jest ważnym elementem programowania w języku Javascript. Poznasz również różne metody, jakie można wykorzystać do tego celu, oraz dowiecie się więcej na temat samego procesu konwersji.

## Jak to zrobić

Aby przekonwertować stringa na małe litery, możemy skorzystać z różnych metod dostępnych w języku Javascript. Poniżej przedstawiamy kilka przykładowych kodów oraz oczekiwane wyjście.

```Javascript
let name = "Jan Kowalski"
console.log(name.toLowerCase())
// output: jan kowalski


let phrase = "WITAŁY SIĘ Z NAMI GOŚCINNE MIĘDZYNARODOWE RÓŻNEGO SZCZYTU MASSONALTY"
console.log(phrase.toLowerCase())
// output: witały się z nami gościnne międzynarodowe różnego szczytu massonalty
```

Istnieje również możliwość wykorzystania pętli do przekonwertowania każdego znaku w stringu na małą literę. Poniżej przykład z użyciem pętli `for`:

```Javascript
let name = "Adam Nowak"
let newName = ""

for (let i=0; i < name.length; i++) {
    newName += name[i].toLowerCase()
}
console.log(newName)
// output: adam nowak
```

Oprócz tego, w języku Javascript mamy też dostęp do metody `String.prototype.toLocaleLowerCase()`, która wykonuje konwersję na podstawie ustawień regionalnych użytkownika. Więcej na ten temat przeczytasz w sekcji "Deep Dive".

## Deep Dive

Konwersja stringa na małe litery jest ważna w wielu aspektach programowania. Przede wszystkim, pozwala nam utrzymać spójność danych i uniknąć błędów w dalszym przetwarzaniu. Dzięki temu, nasz kod staje się bardziej odporny na ewentualne problemy z wielkościami liter.

Jedną z najważniejszych rzeczy, o której warto wspomnieć, jest fakt, że metoda `toLowerCase()` jest niezmienna, czyli nie zmienia oryginalnego stringa, a jedynie zwraca nowy przekonwertowany string. Dzięki temu, mamy możliwość łatwego porównywania oryginalnych i skonwertowanych danych.

Warto również wiedzieć, że metoda `toLocaleLowerCase()` uwzględnia ustawienia regionalne użytkownika, może więc w niektórych przypadkach zwracać nieoczekiwane wyniki. Ważne jest, aby wybrać odpowiednią metodę w zależności od potrzeb naszej aplikacji.

## Zobacz również

- [Dokumentacja metody toLowerCase() w języku Javascript](https://developer.mozilla.org/pl/docs/Web/JavaScript/Referencje/Obiekty/String/toLocaleLowerCase)
- [Porównywanie stringów w języku Javascript](https://www.w3schools.com/js/js_comparisons.asp)
- [Przetwarzanie danych w języku Javascript](https://www.w3schools.com/js/js_data_types.asp)