---
title:                "Rust: Uzyskiwanie bieżącej daty"
programming_language: "Rust"
category:             "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/rust/getting-the-current-date.md"
---

{{< edit_this_page >}}

## Dlaczego

Każdy programista codziennie korzysta z daty, czy to w celu wyświetlenia jej na ekranie czy też do poprawnego indeksowania i sortowania danych. W tym blogu dowiesz się w jaki sposób za pomocą języka Rust możesz uzyskać aktualną datę.

## Jak to zrobić

Pierwszym krokiem do uzyskania aktualnej daty jest importowanie biblioteki `chrono` do Twojego projektu Rust. Następnie, możesz użyć funkcji `Local::now()` aby uzyskać lokalną datę i czas.

```Rust
use chrono::prelude::*;

// Uzyskanie aktualnej daty
let now = Local::now();
println!("{:?}", now);

// Uzyskanie aktualnego czasu
let time = Local::now().time();
println!("{:?}", time);
```

W powyższym przykładzie, wykorzystaliśmy funkcję `now()` z modułu `Local` w bibliotece `chrono`. Dzięki temu otrzymaliśmy aktualną datę i czas w postaci struktury `DateTime` z możliwością wyświetlenia jej za pomocą funkcji `println!()`. Dodatkowo, wykorzystaliśmy funkcję `time()` aby uzyskać tylko aktualny czas.

W przypadku, gdy chcesz uzyskać datę i czas w innym formacie, możesz użyć metody `format()` i podać odpowiedni format jako parametr. Na przykład:

```Rust
// Uzyskanie daty w formacie dd.mm.rrrr
let date = Local::now().format("%d.%m.%Y");
println!("{}", date);
```

W powyższym przykładzie, podaliśmy format daty jako `"%d.%m.%Y"`, gdzie `d` oznacza dzień, `m` miesiąc, a `Y` rok.

## Deep Dive

Biblioteka `chrono` zawiera również wiele innych funkcji i metod, które mogą być przydatne przy operacjach na dacie i czasie. Na przykład, możesz użyć metody `date()` aby uzyskać tylko datę, pomijając czas. Możesz również wykonywać operacje matematyczne na dacie, takie jak dodawanie lub odejmowanie dni przy użyciu funkcji `add()`. Pełną listę wszystkich dostępnych funkcji i metod w bibliotece `chrono` znajdziesz w [dokumentacji](https://docs.rs/chrono/).

## Zobacz też

- [Oficjalna strona języka Rust](https://www.rust-lang.org/pl)
- [Dokumentacja biblioteki chrono](https://docs.rs/chrono/)
- [Przydatne praktyki w programowaniu w Rust](https://mich-cioper.medium.com/rust-przydatne-praktyki-w-programowaniu-d3be89e33a68)