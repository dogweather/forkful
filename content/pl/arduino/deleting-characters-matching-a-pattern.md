---
title:    "Arduino: Usuwanie znaków pasujących do wzorca"
keywords: ["Arduino"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/pl/arduino/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## Dlaczego

Dlaczego ktoś powinien usuwać znaki pasujące do wzorca? Istnieje wiele sytuacji, w których niektóre znaki w ciągu znaków są niepożądane lub niepotrzebne, a usunięcie ich jest pożądane. Przykładowo, w przypadku niektórych urządzeń komunikacyjnych, niektóre znaki muszą zostać usunięte, aby zapewnić poprawną transmisję danych.

## Jak to zrobić


Aby usunąć znaki pasujące do określonego wzorca, można użyć pętli for w połączeniu z instrukcją if, aby sprawdzić każdy znak w ciągu znaków. Jeśli ten znak pasuje do wzorca, można go pominąć lub zastąpić innym znakiem. Przykładowy kod w języku Arduino wyglądałby następująco:

```Arduino
// Pobranie ciągu znaków z portu komunikacyjnego
String receivedString = Serial.readString();

// Przejście przez każdy znak w ciągu znaków
for (int i = 0; i < receivedString.length(); i++) {

  // Sprawdzenie, czy dany znak pasuje do wzorca
  if (receivedString[i] == 'a') {

    // Jeśli pasuje, można go pominąć lub zastąpić innym znakiem
    receivedString.remove(i,1);
  }
}

// Wyświetlenie zmodyfikowanego ciągu znaków
Serial.println(receivedString);
```

Przykładowe wyjście:

```
Przykładowy ciąg znaków: "Zdanie z niechcianym znakiem a!"
Po usunięciu znaku: "Zdnie z niechnim znkiem a!"
```

## Głębsze omówienie

W języku Arduino można również użyć funkcji replace() lub remove() do usunięcia znaków pasujących do wzorca. W przypadku bardziej skomplikowanych wzorców można także wykorzystać wyrażenia regularne za pomocą biblioteki Regexp.

## Zobacz też

1. [Arduino String reference](https://www.arduino.cc/reference/en/language/variables/data-types/stringobject/)
2. [Regexp library for Arduino](https://github.com/aequerd/RegExp)
3. [Arduino String Functions](https://www.tutorialspoint.com/arduino/arduino_strings.htm)