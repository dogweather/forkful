---
title:                "Rust: Wydrukowanie wyjścia z błędami"
programming_language: "Rust"
category:             "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/rust/printing-debug-output.md"
---

{{< edit_this_page >}}

## Dlaczego

Często podczas pisania kodu, programiści muszą debugować swoje aplikacje, aby znaleźć błędy lub zrozumieć, co dokładnie dzieje się w ich programie. Jednym z narzędzi, które może ułatwić ten proces, jest drukowanie informacji debugowych. Dzięki temu, można zweryfikować stan zmiennych, sprawdzić, czy warunki są spełnione i zobaczyć, jak wyglądają wywołane funkcje. Dzięki temu, debugowanie staje się prostsze i szybsze.

## Jak to zrobić

W języku Rust, do drukowania informacji debugowych używa się makra `println!()`. Wewnątrz nawiasów, można umieścić tekst lub wyrażenia, które zostaną wyświetlone na ekranie. Oto przykładowe użycie makra:

```Rust
let name = "Alan";
let age = 29;

println!("Imię: {}", name);
println!("Wiek: {}", age);
```

Wyjście na ekranie będzie wyglądać tak:

```
Imię: Alan
Wiek: 29
```

Makro `println!()` można również używać z formatowaniem, tak jak w języku C. Można użyć specjalnych znaków, aby określić, jakie informacje mają zostać wyświetlone, np. `%s` dla łańcuchów znaków, `%d` dla liczb całkowitych, `%f` dla liczb zmiennoprzecinkowych. Przykład:

```Rust
let num1 = 10;
let num2 = 3.1415;

println!("Liczba całkowita: %d", num1);
println!("Liczba zmiennoprzecinkowa: %.2f", num2);
```

Wyjście na ekranie będzie wyglądać tak:

```
Liczba całkowita: 10
Liczba zmiennoprzecinkowa: 3.14
```

## Głębsze zanurzenie

Makro `println!()` jest bardzo przydatne, ale czasami potrzebujemy więcej niż tylko wyświetlenia tekstu czy wartości zmiennych. W takich przypadkach, może przydać się makro `dbg!()`. To, co odróżnia je od `println!()`, to fakt, że dodatkowo wyświetla również nazwę zmiennej. Przykład:

```Rust
let name = "Alan";
let age = 29;

dbg!(name);
dbg!(age);
```

Wyjście na ekranie będzie wyglądać tak:

```
[name: "Alan"]
[age: 29]
```

Dzięki temu, można szybko sprawdzić, czy nazwa zmiennej jest poprawna i czy zawiera oczekiwaną wartość.

## Zobacz również

- [Dokumentacja Rust: formatowanie tekstu](https://doc.rust-lang.org/book/ch01-02-hello-world.html)
- [Artykuł o debugowaniu w języku Rust](https://www.blakecorbin.com/posts/2016/07/20/some-help-with-debugging-in-rust/)
- [Poradnik na temat użycia makra `dbg!()`](https://blog.skylight.io/bending-the-debugging-curve-with-the-dbg-macro-for-rust/)

Dzięki umiejętności drukowania informacji debugowych, debugowanie w języku Rust staje się łatwiejsze i szybsze. Zapraszamy do wypróbowania tych narzędzi w swoim kodzie!