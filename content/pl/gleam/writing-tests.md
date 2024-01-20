---
title:                "Pisanie testów"
html_title:           "Bash: Pisanie testów"
simple_title:         "Pisanie testów"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/gleam/writing-tests.md"
---

{{< edit_this_page >}}

## Co i dlaczego?
Pisanie testów to sprawdzanie, czy kod robi to, co powinien. Robimy to, żeby szybko znajdować błędy i mieć pewność, że nic się nie zepsuło przy dodawaniu nowych funkcji.

## Jak to zrobić:
Jeśli nie wiesz jak pisać testy w Gleamie, oto przykład. Załóżmy, że mamy funkcję `dodaj` i chcemy przetestować jej działanie.

```gleam
// plik: src/your_module.gleam
pub fn dodaj(a: Int, b: Int) -> Int {
  a + b
}
```

Testy piszemy w folderze `test`. Oto jak może wyglądać test:

```gleam
// plik: test/your_module_test.gleam
import gleam/should
import your_module

pub fn dodaj_test() {
  should.equal(your_module.dodaj(1, 2), 3)
}
```

Uruchom testy komendą `rebar3 eunit`.

## Deep Dive
Pierwsze frameworki do testowania pojawiły się lata temu, ułatwiając życie programistom. W Gleamie, podobnie jak w wielu językach Erlanga VM, popularne jest używanie EUnit. Alternatywą jest często Common Test dla bardziej złożonych przypadków. Detale implementacyjne? Gleam korzysta z asercji z modułu `gleam/should`, łatwo weryfikując poprawność kodu.

## Zobacz też
- [Erlang EUnit](http://erlang.org/doc/apps/eunit/chapter.html)
- [Common Test for Erlang](http://erlang.org/doc/apps/common_test/basics_chapter.html)