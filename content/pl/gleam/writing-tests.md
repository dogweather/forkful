---
title:    "Gleam: Pisanie testów"
keywords: ["Gleam"]
---

{{< edit_this_page >}}

## Dlaczego

Pisanie testów to ważna część procesu programowania, która pomaga zapewnić, że nasz kod jest poprawny i nie zawiera błędów. Testowanie jest także niezbędne do utrzymania jakości naszego kodu w dłuższej perspektywie. W tym artykule omówimy, dlaczego warto pisać testy w języku programowania Gleam.

## Jak to zrobić

Najlepszym sposobem na nauczenie się, jak pisać testy w języku Gleam, jest poprzez przykłady kodu. Poniżej znajdują się przykładowe bloki kodu, których użyliśmy do napisania prostych testów:

```Gleam
fn dodaj(a, b) {
  a + b
}

assert equal(dodaj(2, 3), 5)
assert not_equal(dodaj(2, 3), 6)
```

W powyższym przykładzie funkcja `dodaj` przyjmuje dwa argumenty i zwraca ich sumę. Następnie używamy specjalnej funkcji `assert` wraz z innymi funkcjami, takimi jak `equal` i `not_equal`, aby sprawdzić poprawność zwracanej wartości.

Przykłady takie jak ten pomogą Ci zrozumieć, jak pisać testy w języku Gleam, a także pozwolą Ci na praktyczne zastosowanie tej wiedzy w swoich własnych projektach.

## Głębszy wgląd

Pisanie testów ma wiele korzyści, takich jak zapewnienie poprawności kodu i ułatwienie dalszego rozwoju aplikacji. Ale jest jeszcze jedna ważna kwestia, o której musimy pamiętać - testy powinny być pisane z myślą o wymaganiach naszej aplikacji.

Oznacza to, że musimy wiedzieć, co dokładnie chcemy przetestować i jakie wyniki powinny zostać zwrócone przez nasz kod. Dzięki temu nasze testy będą bardziej efektywne i pozwolą nam znaleźć błędy wczesnym etapie, zanim staną się one większym problemem w przyszłości.

## Zobacz również

- [Dokumentacja Gleam na temat pisania testów](https://gleam.run/documentation/testing)
- [Poradnik na temat pisania testów w języku Gleam](https://medium.com/@sophie_debenedetto/testing-in-gleam-7f22b5e60319)
- [Przykładowy projekt wykorzystujący testy w języku Gleam](https://github.com/gleam-lang/demo-gleam-test)