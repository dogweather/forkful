---
title:                "Drukowanie danych debugowania"
html_title:           "Gleam: Drukowanie danych debugowania"
simple_title:         "Drukowanie danych debugowania"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/gleam/printing-debug-output.md"
---

{{< edit_this_page >}}

## Dlaczego

Drukowanie debug outputu jest nieodłącznym elementem procesu programowania. Dzięki niemu możemy zobaczyć, co dzieje się w naszym kodzie, a także znaleźć ewentualne błędy i poprawić je szybko i skutecznie.

## Jak to zrobić

Gleam jest językiem bardzo przyjaznym dla programistów, również jeśli chodzi o dedukowanie i wyświetlanie debug outputu. Wystarczy użyć funkcji `Debug.todo/1` lub `Debug.inspect/1` przy odpowiednich zmiennych lub wyrażeniach. 

```
Gleam> import Debug

Gleam> number = 42
Gleam> Debug.inspect(number)
42: Number
```

Możemy również użyć funkcji `Debug.format/1` aby sformatować output w określony sposób. 

```
Gleam> fruit = "apple"
Gleam> colour = "red"
Gleam> Debug.format("{fruit} is {colour}", [fruit, colour])
apple is red
```

Gleam również oferuje nam możliwość wyświetlenia debug outputu w funkcjach rekurencyjnych. Wystarczy dodać kolejny parametr do funkcji `Debug.inspect/2`, który będzie reprezentował aktualny poziom rekursji. 

```
fn count_down(number, depth) {
  if number <= 0 {
    Debug.inspect(number, depth)
  } else {
    Debug.inspect(number, depth)
    count_down(number - 1, depth + 1)
  }
}
```

Możesz także użyć funkcji `Debug.todo/2` aby oznaczyć miejsca w kodzie, które chcesz jeszcze uzupełnić. Funkcja ta wyświetli błąd i podpowie Ci gdzie powinieneś dalej pracować. 

```
Gleam> name = "John"
Gleam> Debug.todo("Make a function to greet {name}") 
Error: Unexpected TODO in the code. "Make a function to greet {name}"
```

## Deep Dive

Podczas używania funkcji `Debug.inspect/1`, widzimy obiekt `Gleam.Debug.Output` z opisem wartości i typu. Jest to bardzo przydatne, szczególnie gdy potrzebujemy wyświetlić bardziej złożone struktury danych, jak na przykład krotki czy listy. 

Funkcja `Debug.format/1` działa na podobnej zasadzie jak `Printf` w innych językach programowania. Operujemy na słowach kluczowych (takich jak `{fruit}` w poprzednim przykładzie), których wartości podstawiane są zgodnie z kolejnością w argumencie listy. Dzięki temu możemy swobodniej formatować nasz output. 

W przypadku użycia funkcji `Debug.todo/1` powinniśmy pamiętać, aby w końcowej wersji naszego kodu usunąć wszystkie takie wywołania. W przeciwnym razie zostanie nam zwrócony błąd. 

## Zobacz także

- Oficjalna dokumentacja Gleam dotycząca debugowania: [Gleam - Debugging](https://gleam.run/book/tour/debugging.html)
- Wideo tutorial omawiające debugowanie w Gleam: [Gleam - Debugging with Nuby](https://www.youtube.com/watch?v=sGikLa2yQvA)