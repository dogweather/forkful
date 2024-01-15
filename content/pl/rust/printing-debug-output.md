---
title:                "Wydrukowanie wyników debugowania"
html_title:           "Rust: Wydrukowanie wyników debugowania"
simple_title:         "Wydrukowanie wyników debugowania"
programming_language: "Rust"
category:             "Rust"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/rust/printing-debug-output.md"
---

{{< edit_this_page >}}

## Dlaczego

Jeśli kiedykolwiek pisałeś kod w Rust, na pewno natknąłeś się na polecenie `println!`, które służy do drukowania wartości zmiennych na konsoli. To bardzo przydatne narzędzie do debugowania, więc warto poznać jego możliwości i skutecznie go wykorzystywać.

## Jak to zrobić

W Rust, można użyć polecenia `println!` do drukowania wszelkiego rodzaju wartości, w tym zmiennych, napisów, oraz nawet wyników złożonych wyrażeń:

```Rust
let age = 25;
println!("Mam {} lat", age);
let name = "Jan";
println!("Cześć, jestem {}", name);
let result = 10 * 5;
println!("10 pomnożone przez 5 jest równe {}", result);
```
*Output:*
```
Mam 25 lat
Cześć, jestem Jan
10 pomnożone przez 5 jest równe 50
```

Można również używać formatowania, aby precyzyjnie kontrolować wygląd wydruku, na przykład określając liczbę miejsc po przecinku dla liczb zmiennoprzecinkowych:

```Rust
let pi = 3.14159265359;
println!("Około wartość PI to {:.2}", pi);
```
*Output:*
```
Około wartość PI to 3.14
```

Polecenie `println!` można także wykorzystać do drukowania wielu wartości na raz. W tym przypadku, trzeba będzie użyć formatowania z symbolami `%`, aby określić kolejność wartości:

```Rust
let country = "Polska";
let population = 38_000_000;
println!("W {}, mieszka ponad {} ludzi", country, population);
```
*Output:*
```
W Polska, mieszka ponad 38000000 ludzi
```

## Deep Dive

Polecenie `println!` jest często wykorzystywane do debugowania kodu, ale można również z niego skorzystać w innych sytuacjach, takich jak wyświetlanie prostych komunikatów dla użytkownika lub generowanie raportów. Warto również wspomnieć o poleceniu `eprintln!`, które drukuje taki sam wynik jak `println!`, ale dodaje prefiks "[error]" do wyjścia. Jest to przydatne w przypadku łapania i wypisywania błędów.

## Zobacz także

- [Rust - dokumentacja](https://www.rust-lang.org/pl)
- [Debugowanie w Rust dla początkujących](https://danielkeep.github.io/tlborm/book/README.html)
- [Kurs Rust - Drukuje na ekran ](https://programming-idioms.org/idiom/107/print-to-standard-output/1891/rust)