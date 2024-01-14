---
title:                "Gleam: Wyświetlanie wyników debugowania"
simple_title:         "Wyświetlanie wyników debugowania"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/gleam/printing-debug-output.md"
---

{{< edit_this_page >}}

## Dlaczego

Wiele razy przy pisaniu kodu w języku Gleam napotkasz sytuacje, w których nie jesteś pewien, co właściwie dzieje się w Twoim programie. Często wyświetlanie informacji i wyników podczas debugowania może bardzo pomóc w zrozumieniu działania kodu. W tym krótkim artykule dowiesz się, dlaczego jest to przydatne i jak to zrobić.

## Jak to zrobić

Gleam posiada prostą i skuteczną metodę na wyświetlanie debug outputu. Wystarczy użyć funkcji `debug!()` aby wyświetlić żądaną informację. Poniżej znajduje się przykładowy kod, który demonstruje to w praktyce:

```Gleam
pub fn main() {
  let num = 5
  debug!(num)
}
```

Rezultatem tego kodu będzie wyświetlenie wartości zmiennej `num` w konsoli:

```
5
```

Możesz również wyświetlać dowolne wyrażenia lub wartości zmiennych:

```Gleam
pub fn main() {
  let name = "John"
  let age = 25
  debug!("Imię: " ++ name ++ ", Wiek: " ++ age)
  debug!(age + 5)
}
```

Na konsoli pojawią się następujące wyjścia:

```
Imię: John, Wiek: 25
30
```

## Deep Dive

Istnieje również możliwość wyświetlania danych typu `Record` i `Tuple`. Są one wyświetlane w formacie `Tuple` i posiadają swoją własną unikalną składnię. W przykładzie poniżej `Person` jest rekordem posiadającym pola `name` i `age`:

```Gleam
pub fn main() {
  let person = {
    name: "Adam",
    age: 30
  }
  debug!(person)
}
```

Wyjściem na konsoli będzie:

```
{"Adam", 30}
```

Jeśli chcesz wyświetlić konkretny element w krotce lub rekordzie, możesz użyć `.N` za pomocą którego określisz, który element chcesz wyświetlić (N jest od 0 do końca). Poniżej przykład:

```Gleam
pub fn main() {
  let person = {
    name: "Adam",
    age: 30
  }
  debug!(person.0)
}
```

Konsola wyświetli:

```
{"Adam"}
```

## Zobacz również

- Oficjalna dokumentacja Gleam: https://gleam.run/
- Wprowadzenie do Gleam: https://gleam.run/book/introduction.html