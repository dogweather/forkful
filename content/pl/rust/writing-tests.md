---
title:    "Rust: Pisanie testów"
keywords: ["Rust"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/pl/rust/writing-tests.md"
---

{{< edit_this_page >}}

## Dlaczego

Pisanie testów jest niezbędnym elementem tworzenia solidnego i niezawodnego kodu w języku Rust. Zapewnienie, że nasz kod przechodzi przez wszystkie testy, daje nam pewność, że działa on zgodnie z naszymi oczekiwaniami i minimalizuje ryzyko wystąpienia błędów w przyszłości.

## Jak to zrobić

Język Rust jest wyjątkowo przyjazny dla tworzenia testów i dostarcza nam wiele narzędzi, które ułatwiają ten proces. Pierwszym krokiem jest dodanie adnotacji `#[cfg(test)]` nad naszymi testami oraz dodanie do zależności w pliku `Cargo.toml` biblioteki `test`:

```
Rust
#[cfg(test)]
mod tests {
    #[test]
    fn test_function() {
        //kod testujący
    }
}

Cargo.toml
[dev-dependencies]
test = "0.3.0"
```

Następnie możemy korzystać z makr `assert!` i `assert_eq!` aby sprawdzić czy nasze funkcje zwracają oczekiwane wartości:

```
Rust
#[test]
fn test_addition() {
    let result = add(2, 3);
    assert!(result == 5);
}

#[test]
fn test_subtraction() {
    let result = subtract(10, 5);
    assert_eq!(result, 5);
}
```

Możemy również tworzyć testy dla funkcji, które powinny zwracać błędy, poprzez użycie makra `should_panic`:

```
Rust
#[test]
#[should_panic]
fn test_divide_by_zero() {
    divide(10, 0);
}
```

## Głębsze zagadnienia

Pisanie testów w języku Rust pozwala nam również na dokładne testowanie naszego kodu, w tym także funkcji prywatnych. W tym celu możemy skorzystać z atrybutu `#[derive(Debug)]` i wywołać metodę `inspect()` obiektu, aby zobaczyć wartości jego pól.

```
Rust
#[derive(Debug)]
struct Person {
    name: String,
    age: u32,
    is_adult: bool,
}

impl Person {
    fn new(name: &str, age: u32) -> Self {
        let is_adult = if age >= 18 {
            true
        } else {
            false
        };
        Self {
            name: String::from(name),
            age,
            is_adult,
        }
    }
}

#[test]
fn test_person_inspect() {
    let person = Person::new("John", 25);
    //dodatkowe informacje o tym, co test sprawdza
    println!("name: {:?}, age: {:?}, is_adult: {:?}", person.name, person.age, person.is_adult);
    //otrzymany output
    name: "John", age: 25, is_adult: true
}
```

## Zobacz także

- Dokumentacja Rust o testowaniu: https://doc.rust-lang.org/book/ch11-01-writing-tests.html
- Wprowadzenie do testowania w Rust: https://jamesmunns.com/post/rust-testing-intro/
- Przykładowy projekt z wykorzystaniem testów w Rust: https://github.com/Luminarys/rust-units-example