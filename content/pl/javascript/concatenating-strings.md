---
title:    "Javascript: Łączenie ciągów znaków"
keywords: ["Javascript"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/pl/javascript/concatenating-strings.md"
---

{{< edit_this_page >}}

## Dlaczego

Concatenacja jest jedną z podstawowych operacji w programowaniu, w której łączymy ze sobą dwa lub więcej ciągów znaków. Jest to bardzo przydatne, ponieważ pozwala nam na tworzenie bardziej złożonych i dynamicznych ciągów znaków, co jest niezbędne w wielu aplikacjach internetowych i mobilnych.

## Jak to zrobić

Istnieje wiele sposobów na konkatenację ciągów znaków w języku Javascript. Jedną z najpopularniejszych metod jest użycie operatora "+" lub metody ```concat()```. Przykładowy kod wyglądałby następująco:

```Javascript
// Użycie operatora "+"
var imie = "Jan";
var nazwisko = "Kowalski";
var pelneNazwisko = imie + " " + nazwisko;
console.log(pelneNazwisko); // Wynik: Jan Kowalski

// Użycie metody concat()
var imie = "Anna";
var nazwisko = "Nowak";
var pelneNazwisko = imie.concat(" ", nazwisko);
console.log(pelneNazwisko); // Wynik: Anna Nowak
```

Możemy również użyć tych samych metod do konkatenacji liczb, ale pamiętajmy, że wtedy zostaną one automatycznie przekonwertowane na ciągi znaków.

Inną ciekawą funkcjonalnością jest wykorzystanie operatora "+=" lub metody ```concat()``` do dodawania kolejnych elementów do istniejącego ciągu znaków. Przykładowy kod wyglądałby tak:

```Javascript
var zdanie = "Programowanie w ";
var jezyk = "Javascript";
zdanie += jezyk;
console.log(zdanie); // Wynik: Programowanie w Javascript

var zdanie = "Liczby: ";
var liczby = "123";
zdanie = zdanie.concat(liczby);
console.log(zdanie); // Wynik: Liczby: 123
```

Warto również wspomnieć o tzw. template strings, które pozwalają na łatwiejsze i czytelniejsze tworzenie ciągów znaków z użyciem zmiennych. Przykładowy kod wyglądałby tak:

```Javascript
var imie = "Maria";
var nazwisko = "Nowacka";
var pelneNazwisko = `${imie} ${nazwisko}`;
console.log(pelneNazwisko); // Wynik: Maria Nowacka
```

## Głębsze zagadnienia

Podczas konkatenacji ważne jest, aby pamiętać o odpowiedniej kolejności operacji. Na przykład, jeśli chcemy dodać cyfrę do ciągu znaków, to musimy uważać, aby najpierw przekonwertować liczbę na ciąg znaków. Inaczej, wynik będzie nieprawidłowy.

Warto również zwrócić uwagę na wydajność naszego kodu. W przypadku, gdy często dokonujemy konkatenacji większej liczby ciągów znaków, lepszym wyborem może być użycie metody ```join()```, która jest bardziej wydajna niż połączenie wielu zmiennych przy użyciu operatora "+". 

## Zobacz również

1. [Podstawy języka Javascript](https://developer.mozilla.org/pl/docs/Web/JavaScript/Guide)
2. [Dokumentacja metody concat()](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/String/concat)
3. [Dokumentacja metody join()](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Array/join)