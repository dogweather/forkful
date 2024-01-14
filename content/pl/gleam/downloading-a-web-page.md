---
title:                "Gleam: Pobieranie strony internetowej"
simple_title:         "Pobieranie strony internetowej"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/gleam/downloading-a-web-page.md"
---

{{< edit_this_page >}}

## Dlaczego

Pobieranie stron internetowych jest jedną z podstawowych czynności, które musimy wykonać w naszej codziennej pracy jako programiści. Po przeczytaniu tego artykułu zrozumiesz, dlaczego jest to tak ważne i jak możesz to zrobić za pomocą języka programowania Gleam.

## Jak to zrobić

Aby pobrać stronę internetową za pomocą języka Gleam, możemy użyć biblioteki "httpc", która jest wbudowana w język. Zaczynamy od importowania biblioteki i tworzymy połączenie do wybranej strony:

```Gleam
import httpc
connection = httpc.connect("http://www.przykladowastrona.com")
```

Następnie możemy użyć funkcji "get" w celu pobrania strony i przypisać wynik do zmiennej:

```Gleam
response = httpc.get(connection)
```

Możemy wyświetlić status połączenia przy użyciu funkcji "status" i treść strony przy użyciu funkcji "body":

```Gleam
httpc.status(response) // zwraca 200
httpc.body(response) // zwraca zawartość strony
```

Aby uzyskać informacje o nagłówkach strony, możemy użyć funkcji "headers":

```Gleam
httpc.headers(response) // zwraca wszystkie nagłówki
httpc.get_header(response, "Content-Type") // zwraca wybrany nagłówek
```

## Deep Dive

Chociaż biblioteka "httpc" jest przydatna do prostych pobierania stron internetowych, nie jest to jedyny sposób na to w Gleam. Istnieją inne biblioteki, takie jak "sage" czy "hackney", które oferują bardziej kompleksowe funkcje do pobierania stron. Ponadto, można również wykorzystać biblioteki do parsowania i przetwarzania danych pobranych ze strony.

Zrozumienie działania protokołu HTTP i jego różnych metod jest również kluczowe dla efektywnego pobierania stron internetowych. Pamiętaj, że pobieranie stron internetowych jest tylko jednym z wielu zastosowań dla języka Gleam, który jest pełen innych przydatnych funkcji.

## Zobacz również

- [Dokumentacja biblioteki httpc](https://gleam.run/modules/httpc.html)
- [Dokumentacja biblioteki sage](https://gleam.run/modules/sage.html)
- [Dokumentacja biblioteki hackney](https://gleam.run/modules/hackney.html)