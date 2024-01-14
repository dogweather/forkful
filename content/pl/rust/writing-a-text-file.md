---
title:                "Rust: Tworzenie pliku tekstowego"
programming_language: "Rust"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/rust/writing-a-text-file.md"
---

{{< edit_this_page >}}

## Dlaczego

Pisanie tekstu może sprawiać przyjemność, ale czy kiedykolwiek zastanawiałeś się, jak można zapisać ten tekst do pliku tekstowego? Programowanie może wydawać się trudne, ale dzięki językowi Rust jest to o wiele prostsze i szybsze. Zobacz, jak w łatwy sposób można zapisać tekst do pliku, używając Rust.

## Jak To Zrobić

Aby móc zapisać tekst do pliku w języku Rust, potrzebujemy w naszym kodzie użyć biblioteki standardowej `std::fs`. Najpierw musimy utworzyć instancję struktury `File`, która reprezentuje plik tekstowy. Następnie, za pomocą metody `write` możemy przesłać tekst do naszego pliku. Poniżej przedstawiamy przykładowy kod, który zapamiętuje nasz tekst "Cześć, świecie!" do pliku o nazwie "tekst.txt":

```Rust
use std::fs::File;
use std::io::prelude::*;

fn main() {
    let mut plik = File::create("tekst.txt").unwrap();
    plik.write(b"Cześć, świecie!").unwrap();
}
```

Po uruchomieniu tego kodu, zostanie utworzony plik tekstowy "tekst.txt", który będzie zawierał tekst "Cześć, świecie!". W przypadku, gdy plik już istnieje, metoda `create` zwróci błąd, dlatego używamy metody `unwrap`, aby go obsłużyć.

## Deep Dive

Kod użyty w przykładzie jest dość prosty, jednak istnieje wiele innych sposobów na zapis tekstu do pliku w języku Rust. Możemy na przykład użyć metody `writeln`, aby automatycznie dodać znak nowej linii na końcu tekstu, lub użyć makra `writeln!`, które oprócz tekstu będzie mogło również przyjmować zmienne.

Warto również zaznaczyć, że metoda `write` zwraca wynik w postaci `io::Result`, który jest typem enum z dwiema wartościami: `Ok` i `Err`. Dzięki temu możemy lepiej kontrolować nasz program i obsługiwać ewentualne błędy zapisu.

## Zobacz również

- [Dokumentacja biblioteki standardowej fs](https://doc.rust-lang.org/std/fs/index.html)
- [Inne możliwości zapisywania danych w Rust](https://blog.logrocket.com/7-ways-to-write-data-in-rust/)
- [Poradnik języka Rust od podstaw](https://pl.wikibooks.org/wiki/Rust/J%C4%99zyk_Rust:_Wst%C4%99p)