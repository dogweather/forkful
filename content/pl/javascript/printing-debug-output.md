---
title:                "Wyświetlanie wyników debugowania"
html_title:           "Javascript: Wyświetlanie wyników debugowania"
simple_title:         "Wyświetlanie wyników debugowania"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/javascript/printing-debug-output.md"
---

{{< edit_this_page >}}

## Dlaczego

Debugowanie to nieodłączna część pisania kodu, a w niektórych sytuacjach często musimy zrozumieć, co dzieje się w trakcie jego wykonania. Właśnie wtedy przydatne mogą okazać się komendy do drukowania zawartości zmiennych i innych informacji o naszym kodzie. W tym artykule dowiesz się, dlaczego drukowanie debugu jest przydatne i jak możesz to zrobić w JavaScript.

## Jak to zrobić

Aby wydrukować debug output w JavaScript, możesz skorzystać z funkcji `console.log()`. Przykładowo, jeśli chcesz wydrukować wartości zmiennej `x`, wykonaj następujący kod:

```Javascript
let x = 5;
console.log(x);
```

W wyniku otrzymasz `5` w konsoli przeglądarki. Oprócz zmiennych, możesz także wydrukować dowolny tekst, korzystając z interpolacji stringów, jak pokazano w poniższym przykładzie:

```Javascript
let name = 'Adam';
console.log(`Witaj, ${name}!`);
```

W konsoli pojawi się `Witaj, Adam!`. Dzięki temu, możesz wyświetlić informacje o błędach, wartościach zmiennych, czy też po prostu potwierdzić, że dany kod został wykonany.

## Deep Dive

Funkcja `console.log()` jest bardzo pomocna, ale w niektórych sytuacjach możesz potrzebować bardziej zaawansowanych narzędzi do debugowania. Wtedy warto zapoznać się z innymi funkcjami dostępnymi w JavaScript, takimi jak `console.error()`, `console.warn()` czy `console.time()`. Dzięki nim możesz łatwiej zlokalizować i naprawić ewentualne błędy w swoim kodzie.

Oprócz tego, warto także poznać narzędzia do debugowania w przeglądarkach, takie jak konsola deweloperska czy debugger, które oferują dodatkowe funkcje, takie jak wyświetlanie wartości zmiennych w trakcie działania aplikacji.

## Zobacz także

- [Dokumentacja funkcji `console` w JavaScript](https://developer.mozilla.org/pl/docs/Web/API/Console)
- [Podstawy debugowania w JavaScript](https://codeburst.io/javascript-debugging-for-beginners-a4b6f0d76fad)
- [Narzędzia debugowania w przeglądarkach](https://blog.logrocket.com/debugging-javascript-in-chrome-like-a-pro/)