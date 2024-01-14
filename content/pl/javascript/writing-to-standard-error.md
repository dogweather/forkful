---
title:                "Javascript: Pisanie do standardowego błędu"
simple_title:         "Pisanie do standardowego błędu"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/javascript/writing-to-standard-error.md"
---

{{< edit_this_page >}}

### Dlaczego warto używać standardowego wyjścia błędu w programowaniu Javascript?

Często podczas pisania kodu, natrafiamy na błędy, które uniemożliwiają poprawne działanie naszej aplikacji. W takich sytuacjach bardzo pomocne jest używanie standardowego wyjścia błędu w Javascript. Dzięki temu możemy szybko zlokalizować problem i naprawić go, co przyczyni się do poprawnego działania naszego programu.

### Jak używać standardowego wyjścia błędu w Javascript?

Aby móc korzystać ze standardowego wyjścia błędu w Javascript, musimy znać kilka prostych komend. Pierwszą z nich jest ```console.error()```, która pozwala nam na wypisanie błędu w konsoli przeglądarki. Przykładowo:

```Javascript
let num = "abc";

if(isNaN(num)){
    console.error("Wprowadzony argument nie jest liczbą");
}
```

W tym przykładzie, wprowadzenie tekstu zamiast liczby do zmiennej "num", spowoduje wypisanie błędu w konsoli: "Wprowadzony argument nie jest liczbą".

Możemy także użyć ```process.stderr.write()```, aby wypisać błąd w terminalu. Przykład:

```Javascript
process.stderr.write("Błąd: Nie można odnaleźć pliku");
```

Ten kod spowoduje wypisanie tekstu "Błąd: Nie można odnaleźć pliku" w terminalu.

### Głębszy wgląd w wykorzystanie standardowego wyjścia błędu w Javascript

Używanie standardowego wyjścia błędu jest nie tylko przydatne w sytuacjach, gdy występują błędy w naszym kodzie. Możemy też wykorzystać to narzędzie do debugowania naszej aplikacji. W przypadku, gdy chcemy śledzić pewne wartości w naszym programie, możemy użyć ```console.error()``` lub ```process.stderr.write()``` do wypisywania tych wartości w konsoli lub terminalu. Dzięki temu możemy szybko sprawdzić, czy nasze zmienne mają poprawne wartości i czy nasz kod działa zgodnie z oczekiwaniami.

### Zobacz także

- [Jak używać standardowego wyjścia błędu w Javascript](https://developer.mozilla.org/pl/docs/Web/API/console/error)
- [Tutorial: Debugowanie aplikacji Javascript z wykorzystaniem standardowego wyjścia błędu](https://www.digitalocean.com/community/tutorials/how-to-debug-node-js-errors)
- [Informacje o module "process" w Node.js](https://nodejs.org/docs/latest/api/process.html)