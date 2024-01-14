---
title:    "Rust: Tworzenie pliku tekstowego"
keywords: ["Rust"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/pl/rust/writing-a-text-file.md"
---

{{< edit_this_page >}}

## Dlaczego

W dzisiejszych czasach programowanie stało się jedną z najważniejszych umiejętności, a język Rust zyskuje coraz większą popularność wśród programistów. Istnieje wiele powodów, dla których warto zainteresować się tym językiem, ale jednym z najważniejszych jest jego bezpieczeństwo i wydajność. W dzisiejszym artykule opowiemy o jednym z podstawowych zadań programistycznych - pisaniu tekstu do pliku - i jak można to osiągnąć przy użyciu języka Rust.

## Jak to zrobić

Aby napisać tekst do pliku w języku Rust, musimy najpierw zaimportować bibliotekę "std::fs" za pomocą kodu "use std::fs;". Następnie, wykorzystując metodę "fs::write", możemy wpisać nasz tekst do pliku. Aby zobaczyć efekt, musimy otworzyć ten plik w trybie odczytu. Poniżej znajduje się przykładowy kod i wynik.

```Rust
use std::fs;

fn main() {
    let text = "Cześć! Jestem tekstem, który zostanie zapisany do pliku.";
    fs::write("test.txt", text).expect("Nie udało się zapisać tekstu do pliku"); 
    
    let read_text = fs::read_to_string("test.txt").expect("Nie udało się odczytać tekstu z pliku");
    
    println!("Tekst w pliku test.txt to: {}", read_text);
}
```

**Wynik:** Tekst w pliku test.txt to: Cześć! Jestem tekstem, który zostanie zapisany do pliku.

## Głębszy wgląd

Pisząc tekst do pliku w języku Rust, musimy pamiętać o możliwościach obsługi błędów. W przykładzie powyżej została wykorzystana metoda "expect()", która wyświetli błąd w przypadku niepowodzenia operacji. Jest to prosta metoda, ale może nie być wystarczająca w bardziej złożonych programach. Dlatego warto zapoznać się z innymi metodami obsługi błędów, takimi jak "Result" czy mechanizmy "panic" i "unwrap" dostępne w języku Rust.

## Zobacz także

Jeśli chcesz dowiedzieć się więcej o pisaniu tekstu do pliku w języku Rust, polecamy zapoznanie się z oficjalną dokumentacją języka i odwiedzenie innych ciekawych stron:

- Dokumentacja języka Rust: https://doc.rust-lang.org/book/ch09-02-recoverable-errors-with-result.html
- Przykład obsługi błędów w języku Rust: https://blog.logrocket.com/handling-errors-in-rust/
- Poradnik dla początkujących w języku Rust: https://medium.com/@veswill3/getting-started-with-rust-streaming-files-1177ce45600c