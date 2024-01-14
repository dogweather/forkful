---
title:    "Rust: Znajdowanie długości ciągu znaków"
keywords: ["Rust"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/pl/rust/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## Dlaczego

Programowanie w Rust jest niezwykle popularne wśród programistów. Jednym z najważniejszych aspektów tego języka programowania jest jego wydajność i bezpieczeństwo. Jednak oprócz tego, Rust oferuje również wiele przydatnych funkcji, takich jak wyznaczanie długości napisów. W tym artykule dowiesz się, dlaczego znajomość sposobu wyznaczania długości napisów jest ważna i jak to zrobić.

## Jak wyznaczyć długość napisu w Rust?

Aby uzyskać długość napisu w Rust, możemy użyć metody `.len()`. Przykład użycia tej metody wygląda następująco:

```Rust
let napis = "To jest przykładowy napis";
println!("Długość napisu to: {}", napis.len());
```

W powyższym przykładzie, metoda `.len()` jest użyta na zmiennej `napis`, która zawiera napis "To jest przykładowy napis". Jej wynik zostaje wyświetlony w konsoli i w tym przypadku będzie to: `Długość napisu to: 24`.

## Głębsze spojrzenie

Okazuje się, że metoda `.len()` jest tylko jedną z możliwych metod wyznaczania długości napisów w Rust. Inną użyteczną metodą jest `.chars().count()`, która zwraca liczbę znaków w napisie. Przykład użycia:

```Rust
let napis = "To jest przykładowy napis";
println!("Liczba znaków w napisie to: {}", napis.chars().count());
```

W powyższym przykładzie, metoda `.chars().count()` zostaje użyta na zmiennej `napis`, a jej wynik również wynosi 24.

Ponadto, Rust oferuje również sposoby wyznaczania długości napisów w różnych kodowaniach, takich jak UTF-8 czy UTF-16. Aby dowiedzieć się więcej na ten temat, warto zagłębić się w dokumentację języka lub poszukać innych przydatnych źródeł.

## Zobacz również

- Dokumentacja Rust: https://doc.rust-lang.org/std/primitive.str.html#method.len
- Tutoriale i przykłady wideo na temat programowania w Rust: https://www.rust-lang.org/learn
- Blog polskiej społeczności Rust: https://blog.rust-lang.org/pl/