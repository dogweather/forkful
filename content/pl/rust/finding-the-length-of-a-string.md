---
title:                "Rust: Znajdowanie długości ciągu znaków"
programming_language: "Rust"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/rust/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## Dlaczego

Jeśli jesteś programistą lub chcesz zostać jednym, prawdopodobnie już słyszałeś o Rust - nowoczesnym, szybkim i niezawodnym języku programowania. Jedną z wielu zalet tego języka jest to, że jest on bardzo efektywny w zarządzaniu pamięcią. W tym szkoleniu przeanalizujemy, jak znaleźć długość łańcucha znaków w Rust i jak możesz wykorzystać tę wiedzę w swoich projektach.

## Jak to zrobić

Aby znaleźć długość łańcucha znaków w Rust, musimy wykorzystać metodę `len()` dla typu `String`. Wywołanie tej metody zwróci liczbę znaków w danym łańcuchu. Oto przykładowy kod:

```Rust
let s = String::from("Cześć, jestem tekstem!");
let len = s.len();
println!("Długość łańcucha: {}", len);
```

Output: `Długość łańcucha: 21`

Możesz także wykorzystać metodę `len()` dla typu `str`, który jest częścią Rustowego typu `&str`, który reprezentuje łańcuchy znaków stałych. Przykładowy kod:

```Rust
let s = "Witaj, świecie!";
let len = s.len();
println!("Długość łańcucha: {}", len);
```

Output: `Długość łańcucha: 15`

## Głębszy przegląd

Warto zauważyć, że metoda `len()` dla typu `String` zwraca liczbę bajtów, a nie liczbę znaków. W niektórych językach, taka jak Java czy C#, długość łańcucha jest zwracana jako liczba znaków. W Rust, dlatego że łańcuchy znaków są kodowane przy użyciu UTF-8, nie ma gwarancji, że jedna litera odpowiada jednej wartości bajtowej. Może to prowadzić do nieoczekiwanych wyników. Dlatego zawsze warto sprawdzić, czy długość łańcucha jest zgodna z oczekiwaniami.

## Zobacz także

Jeśli chcesz dowiedzieć się więcej o pracowaniu z łańcuchami znaków w Rust, zapoznaj się z oficjalną dokumentacją: [https://doc.rust-lang.org/std/string](https://doc.rust-lang.org/std/string)

Jeśli chcesz dowiedzieć się więcej o języku Rust, zapraszamy na stronę: [http://rust-lang.org/](http://rust-lang.org/)

Jeśli potrzebujesz pomocy lub chcesz podzielić się swoimi doświadczeniami z innymi programistami Rust, dołącz do społeczności na forum: [https://users.rust-lang.org/](https://users.rust-lang.org/)