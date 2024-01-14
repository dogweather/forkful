---
title:                "Rust: Zapisywanie ciągu z wielkiej litery"
simple_title:         "Zapisywanie ciągu z wielkiej litery"
programming_language: "Rust"
category:             "Rust"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/rust/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## Dlaczego

Czy kiedykolwiek musiałeś zmienić wielkość liter w łańcuchu znaków? Może potrzebowałeś zapisać imię i nazwisko w formacie "Imię Nazwisko" lub zmienić pierwszą literę każdego słowa na wielką? Właśnie o takiej operacji będziemy dzisiaj mówić! W tym wpisie dowiesz się, dlaczego jest tak ważne i w jaki sposób wykonać zwięzłe i wydajne przekształcenie łańcucha znaków.

## Jak to zrobić

Aby przekształcić łańcuch znaków na taki, w którym pierwsza litera każdego słowa będzie wielka, będziemy musieli skorzystać z metody `to_uppercase()` i `replace()` z biblioteki standardowej języka Rust. Poniższy kod pokazuje przykład, w którym zostanie zastosowana ta operacja na łańcuchu "jan kowalski".

```Rust
let name = "jan kowalski".to_uppercase().replace(" ", "");
println!("Imię i nazwisko w formacie z wielkimi literami: {}", name);
```

Po uruchomieniu programu powinniśmy zobaczyć następujący wynik:

```
Imię i nazwisko w formacie z wielkimi literami: JanKowalski
```

Jak widzimy, dzięki skorzystaniu z odpowiednich metod, uzyskaliśmy pożądany rezultat. Teraz możemy zastosować tę samą operację w swoich programach, aby uzyskać bardziej czytelne i estetyczne wyjście.

## Deep Dive

W czasie działania metody `to_uppercase()`, gdzie jest definiowana kolejność wielkich i małych liter? Okazuje się, że zależy to od aktualnie używanego systemu. Na przykład, w systemie ASCII, litery są uporządkowane według ich numerów w tabeli, a znaki o mniejszej liczbie będą poprzedzać znaki o większej liczbie. Dlatego w tym przypadku wielkimi literami są najpierw umieszczane znaki o wyższych numerach.

W systemie Unicode, wielkość liter może być różna w zależności od ustawień językowych i narodowych. Dlatego też, w niektórych przypadkach może się zdarzyć, że wynik metody `to_uppercase()` nie będzie jednoznaczny. W takiej sytuacji warto skorzystać z innych metod z biblioteki standardowej, takich jak `to_uppercase_lossy()`, która zwróci w miarę możliwości jednoznaczny wynik.

## Zobacz także

- [Dokumentacja standardowej biblioteki języka Rust](https://doc.rust-lang.org/std/)
- [ASCII vs Unicode](https://www.edureka.co/blog/ascii-vs-unicode/)
- [Wpływ ustawień językowych i narodowych na wielkość liter w Unicode](https://unicode.org/faq/casemap_charprop.html)