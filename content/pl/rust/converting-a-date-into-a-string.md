---
title:    "Rust: Przetwarzanie daty na ciąg znaków"
keywords: ["Rust"]
---

{{< edit_this_page >}}

## Dlaczego warto konwertować datę na ciąg znaków?

Konwersja daty na ciąg znaków może być niezbędna w wielu sytuacjach. Na przykład, jeśli pracujesz z danymi, które muszą być łatwe do odczytania przez użytkowników lub systemy zewnętrzne, konwersja na czytelny format jest niezbędna. Może to również pomóc w wyświetlaniu dat w różnych językach, zależnie od preferencji użytkownika.

## Jak to zrobić w języku Rust?

Aby skonwertować datę na ciąg znaków w Rust, najprostszym sposobem jest użycie funkcji "format!" z makrem "println!". Oto kilka przykładowych kodów i wyników:

```Rust
use chrono::NaiveDate;

fn main() {
    let date = NaiveDate::from_ymd(2020, 06, 22);
    let string_date = format!("{:?}", date.format("%d/%m/%Y"));
    println!("Data jako ciąg znaków: {}", string_date);
}
```

```
Data jako ciąg znaków: 22/06/2020
```

W powyższym kodzie najpierw musimy zaimportować bibliotekę "chrono", która pomaga w manipulacji datami. Następnie definiujemy datę przy użyciu metody "from_ymd", a następnie korzystamy z metody "format" wraz z odpowiednim formatowaniem, aby uzyskać pożądany wygląd daty. W końcu, dzięki funkcji "format!", konwertujemy datę na ciąg znaków i wyświetlamy ją przy użyciu makra "println!".

## Wnikliwsze spojrzenie na konwersję daty na ciąg znaków

Warto również wspomnieć o możliwości wykorzystania biblioteki "chrono" do konwersji daty na ciąg znaków w konkretnym formacie, który odpowiada określonemu językowi czy lokalizacji. W tym przypadku, należy użyć metody "format_localized" zamiast "format", a następnie podać odpowiedni język lub lokalizację w parametrze metody.

Ponadto, biblioteka "chrono" oferuje wiele innych przydatnych funkcji i metod związanych z manipulacją datami, więc warto ją pogłębienie badać, aby wykorzystać ją w pełni.

## Zobacz również

- Dokumentacja biblioteki "chrono" dotycząca konwersji daty na ciąg znaków: https://docs.rs/chrono/0.4.19/chrono/format/strftime/index.html
- Przykładowe kody i wyjaśnienie konwersji daty na ciąg znaków w języku Rust: https://dev.to/maxime_chantry/date-time-and-duration-in-rust-527d
- Wideo tutorial na temat konwertowania daty na ciąg znaków w Rust: https://www.youtube.com/watch?v=Ia6WC4nb4i8