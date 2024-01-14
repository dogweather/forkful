---
title:    "Rust: Wydrukowanie danych do debugowania"
keywords: ["Rust"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/pl/rust/printing-debug-output.md"
---

{{< edit_this_page >}}

Dlaczego: Dlaczego warto wypisywać informacje debugowania w Rust?

Rust to stosunkowo młody język programowania, który jest wykorzystywany w wielu różnych projektach. Jedną z jego najważniejszych cech jest bezpieczeństwo - dzięki surowym i ścisłym regułom językowi Rust udało się stworzyć narzędzia, które pozwalają wykryć błędy na etapie kompilacji, zanim program trafi do użytkowników. Jednak, jak każdy program, także i te napisane w Rust mogą zawierać błędy. W takiej sytuacji bardzo pomocna jest możliwość wypisywania informacji debugowania, które pozwalają nam śledzić przepływ programu i sprawdzić co mogło pójść nie tak. W tym artykule pokażemy, jak w prosty sposób wypisywać informacje debugowania w języku Rust.

Jak to zrobić?

Aby wypisać informacje debugowania w Rust, możemy użyć funkcji "println!", która pozwala nam wypisać informacje na standardowym wyjściu. Możemy również wykorzystać makro "dbg!" (dostępne od wersji Rust 1.32), które służy do wypisywania informacji debugowania na standardowym wyjściu, a także zwraca wartość zmiennej, która została przekazana jako argument. 

Przykładowo, jeśli chcemy wypisać wartość zmiennej "x", możemy użyć funkcji "println!("Wartość zmiennej x: {}", x);" lub makra "dbg!(x);". W obu przypadkach, na standardowym wyjściu zostanie wypisana wartość zmiennej "x".

Jeśli chcemy wypisać wiele zmiennych na raz, możemy użyć funkcji "println!", przekazując jako argument tuplę zmiennych. Natomiast w przypadku użycia makra "dbg!", musimy oddzielić zmienne przecinkami, na przykład: "dbg!(x, y, z);".

Deep Dive: Głębsze informacje o wypisywaniu informacji debugowania w Rust

Funkcja "println!" i makro "dbg!" są tylko jednymi z wielu narzędzi do wypisywania informacji debugowania w języku Rust. Istnieją również inne funkcje i makra, takie jak "eprintln!", "format!" czy "log!", które pozwalają na bardziej zaawansowane wypisywanie informacji debugowania. W przypadku funkcji "eprintln!", informacje zostaną wypisane na standardowym wyjściu błędów, natomiast makro "log!" pozwala nam decydować, na jakim poziomie chcemy wypisać informacje.

Ponadto, istnieje możliwość zdefiniowania własnych makr, które pozwolą nam na jeszcze bardziej dopasowane wypisywanie informacji debugowania do naszych potrzeb.

See Also: Zobacz także

- Dokumentacja języka Rust (https://doc.rust-lang.org/)
- Tutorial o wypisywaniu informacji debugowania w Rust (https://www.educative.io/blog/rust-debugging-tips)
- Przykładowe projekty w języku Rust (https://github.com/rust-lang/rust/wiki/Notable-Rust-Projects)