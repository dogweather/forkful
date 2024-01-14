---
title:                "Rust: Tworzenie pliku tekstowego"
simple_title:         "Tworzenie pliku tekstowego"
programming_language: "Rust"
category:             "Rust"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/rust/writing-a-text-file.md"
---

{{< edit_this_page >}}

## Dlaczego

Pisanie kodu może być fascynującym wyzwaniem, a w szczególności pisanie w języku Rust. Jedną z przydatnych umiejętności w programowaniu jest tworzenie plików tekstowych. W tym artykule dowiesz się, dlaczego warto nauczyć się tego w języku Rust.

## Jak to zrobić

Początek jest prosty, musisz tylko utworzyć nowy projekt Rust i utworzyć plik o rozszerzeniu `.rs`. Następnie będziesz miał możliwość używania wielu funkcji i metod, aby utworzyć swój własny plik tekstowy. Oto przykładowy kod:

```Rust
use std::fs::File;
use std::io::prelude::*;

fn main() {
  let mut file = File::create("moj_plik.txt").expect("Nie można utworzyć pliku");
  file.write_all(b"To jest pierwszy tekst w moim pliku tekstowym!").expect("Nie można zapisać do pliku");
}
```

W powyższym przykładzie importujemy bibliotekę `std::fs::File`, która pozwala nam na tworzenie i manipulowanie plikami, oraz bibliotekę `std::io::prelude:: *`, która udostępnia metody do czytania i pisania do pliku. Następnie w funkcji `main` tworzymy nowy plik o nazwie `moj_plik.txt` i piszemy w nim tekst. Zauważ, że tekst jest konwertowany na typ `byte` za pomocą `b"..."`.

Po uruchomieniu programu w Twoim środowisku Rust, utworzony zostanie plik o nazwie `moj_plik.txt` w folderze z projektem i będzie zawierać tekst "To jest pierwszy tekst w moim pliku tekstowym!".

## Wgląd w głębsze szczegóły

W powyższym przykładzie użyliśmy metody `write_all` do zapisania tekstu do pliku tekstowego. Ale istnieje wiele innych metod, które można wykorzystać w celu dostosowania swojego pliku. Na przykład, możesz użyć metody `append`, aby dodać nowy tekst do istniejącego pliku, lub `seek` do ustawienia kursora w konkretnym miejscu w pliku.

## Zobacz również

Jeśli chcesz dowiedzieć się więcej o tworzeniu plików tekstowych w języku Rust, możesz przejść przez oficjalną dokumentację na stronie https://doc.rust-lang.org/std/fs/struct.File.html. Będzie to świetny sposób na pogłębienie swojej wiedzy na ten temat.

Możesz również przeczytać inne artykuły ze strony https://www.rust-lang.org/learn, aby nauczyć się więcej o programowaniu w języku Rust. Nie bój się eksperymentować i tworzyć własnych projektów, aby lepiej zrozumieć ten język programowania. Powodzenia!