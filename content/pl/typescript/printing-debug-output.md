---
title:                "TypeScript: Wyświetlanie danych debugowania"
simple_title:         "Wyświetlanie danych debugowania"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/typescript/printing-debug-output.md"
---

{{< edit_this_page >}}

## Dlaczego

Często podczas tworzenia programów w języku TypeScript, przydaje się drukowanie wiadomości debugujących, aby zrozumieć, co dzieje się w naszym kodzie i w razie potrzeby go naprawić. W tym artykule przeczytasz, dlaczego warto używać drukowania debug output oraz jak to zrobić w praktyce.

## Jak to zrobić

Aby wyświetlić debug output w języku TypeScript, możesz skorzystać z metody `console.log`. W poniższym przykładzie używamy tej metody do wyświetlenia tekstu "Witaj świecie!".

```TypeScript
console.log('Witaj świecie!');
```
W terminalu powinno zostać wyświetlone:

```
Witaj świecie!
```

Możesz również drukować zmienne, aby sprawdzić ich wartości. Na przykład, jeśli chcesz sprawdzić wartość zmiennej `liczba`, możesz napisać:

```TypeScript
let liczba = 10;
console.log('Wartość zmiennej liczba to: ' + liczba);
```
W efekcie otrzymasz:

```
Wartość zmiennej liczba to: 10
```

## Deep Dive

Drukowanie debug output jest nie tylko przydatne, ale także ważne podczas debugowania kodu. Może pomóc zlokalizować błędy lub zrozumieć, dlaczego pewne rzeczy nie działają tak, jak powinny. Dzięki temu możesz szybciej naprawić problemy i ułatwić sobie pracę.

Pamiętaj jednak, że drukowanie debug output nie jest zalecane w kodzie produkcyjnym, ponieważ może spowolnić działanie aplikacji lub wyświetlać niepotrzebne informacje użytkownikom.

## Zobacz również

- [Dokumentacja TypeScript](https://www.typescriptlang.org/)
- [Console API w TypeScript](https://developer.mozilla.org/en-US/docs/Web/API/Console)
- [Narzędzia do debugowania TypeScript](https://marketplace.visualstudio.com/items?itemName=msjsdiag.debugger-for-chrome)