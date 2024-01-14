---
title:                "Javascript: Drukowanie wyników debugowania"
simple_title:         "Drukowanie wyników debugowania"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/javascript/printing-debug-output.md"
---

{{< edit_this_page >}}

## Dlaczego

Na co dzień programiści wykonują wiele zadań, a jednym z nich jest debugowanie kodu. Wiele osób uważa to za frustrujące, ale jest to nieunikniony proces, aby upewnić się, że nasz program działa zgodnie z oczekiwaniami. Jednym z narzędzi, które pomagają w tym procesie, jest drukowanie komunikatów debugowania. Pozwala ono programiście zobaczyć, co dokładnie dzieje się w kodzie i gdzie może być występujący problem. Dlatego warto poznać, jak to zrobić i jak może to ułatwić debugowanie.

## Jak to zrobić

Aby wyświetlić komunikaty debugowania w swoim kodzie, wystarczy użyć funkcji `console.log()`. Jest to wbudowana funkcja dostępna we wszystkich przeglądarkach internetowych i środowiskach programistycznych opartych na JavaScript. Poniżej znajduje się przykładowy kod z użyciem funkcji `console.log()` oraz jego wynik w konsoli.

```Javascript
let number = 7
console.log("Wartość zmiennej number wynosi: " + number);
```
Wynik w konsoli:
```
Wartość zmiennej number wynosi: 7
```

Możemy również drukować wartości zmiennych wewnątrz pętli lub warunków, aby śledzić, jak zmieniają się podczas wykonywania kodu. Przykładowy kod z użyciem pętli `for` i funkcji `console.log()`:

```Javascript
for (let i = 0; i <= 5; i++) {
  console.log("Aktualna wartość pętli wynosi: " + i);
}
```
Wynik w konsoli:
```
Aktualna wartość pętli wynosi: 0
Aktualna wartość pętli wynosi: 1
Aktualna wartość pętli wynosi: 2
Aktualna wartość pętli wynosi: 3
Aktualna wartość pętli wynosi: 4
Aktualna wartość pętli wynosi: 5
```

Dodatkowo, funkcja `console.log()` może przyjmować więcej niż jedną wartość, dzięki czemu możemy drukować kilka wartości jednocześnie. Przykładowy kod:

```Javascript
let name = "Anna";
let age = 25;
let job = "programistka";

console.log("Cześć, jestem " + name + ", mam " + age + " lat i pracuję jako " + job + ".");
```
Wynik w konsoli:
```
Cześć, jestem Anna, mam 25 lat i pracuję jako programistka.
```

## Zapoznaj się z szczegółami

Istnieje wiele narzędzi i technik, które mogą ułatwić drukowanie komunikatów debugowania. Na przykład, możemy użyć `console.error()` do wyświetlenia błędów lub `console.table()` do wyświetlenia danych w postaci tabeli. Możemy również zamiast funkcji `console.log()` użyć bardziej zaawansowanych narzędzi, takich jak debugger dostępny w większości przeglądarek. Warto również pamiętać o śledzeniu wydajności naszego kodu i wyświetlaniu odpowiednich komunikatów w celu zidentyfikowania potencjalnych problemów.

## Zobacz również

- [Console API](https://developer.mozilla.org/pl/docs/Web/API/Console)
- [Debugowanie w przeglądarkach](https://developer.mozilla.org/pl/docs/Tools)
- [Techniki debugowania w JavaScript](https://javascript.pl/tutorial/debugowanie)