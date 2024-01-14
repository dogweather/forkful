---
title:                "Rust: Wyszukiwanie i zamienianie tekstu"
programming_language: "Rust"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/rust/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## Dlaczego

Wybranie i zamiana tekstu jest jednym z najczęstszych zadań wykonywanych przez programistów. Może to być wymagane podczas refaktoryzacji kodu, zmiany nazw zmiennych lub funkcji, lub po prostu poprawenia literówek w plikach źródłowych. W tym poście dowiecie się, jak w prosty sposób zamieniać tekst w języku Rust.

## Jak to zrobić

W języku Rust istnieje wiele różnych sposobów na wybranie i zamianę tekstu, ale najprostszym i najczęściej używanym jest użycie metody `replace()` dostępnej dla typu `String`. Przypomnijmy sobie podstawowe zasady działania tej metody za pomocą poniższego przykładu:

```Rust
let text = "Witaj, świecie!";
let new_text = text.replace("świecie", "Ruscie");
println!("{}", new_text);

// Output: Witaj, Ruscie!
```

W tym przypadku wywołujemy metodę `replace()` na zmiennej `text`, przekazując jako pierwszy argument tekst, który chcemy zamienić, a jako drugi - tekst, na który chcemy go zamienić. Następnie, za pomocą funkcji `println!()`, wypisujemy wynik.

Ale co jeśli chcielibyśmy dokonać zamiany jednocześnie na różnych fragmentach tekstu? W takim przypadku możemy skorzystać z metody `replace_range()`, która pozwala nam podać wybrany zakres tekstu do zamiany. Przykład wyglądałby następująco:

```Rust
let mut text = String::from("Witaj, świecie!");
text.replace_range(7.., "bezpieczny");
println!("{}", text);

// Output: Witaj, bezpieczny!
```

W tym przypadku wykorzystujemy metodę `replace_range()` na zmiennej `text`, przekazując jako pierwszy argument początkowy indeks, od którego chcemy dokonać zamiany, a jako drugi - tekst, który chcemy wstawić. Ponadto, ustawiamy zmienną `text` jako mutowalną, aby móc dokonać zmiany na jej wartości.

## Przypadki specjalne

Co w sytuacji, gdy chcemy dokonać zamiany tekstu, ale nie chcemy uwzględniać wielkości liter? Tutaj przydatna okaże się metoda `replace()` wraz z funkcją `to_lowercase()`, która zmienia wszystkie litery w tekście na małe. Przykładowy kod wyglądałby tak:

```Rust
let text = "HELLO, WORLD!";
let new_text = text.replace("hello", "cześć");
println!("{}", new_text);

// Output: HELLO, WORLD!
```

W tym przypadku wprowadzone zmiany nie zostaną uwzględnione, ponieważ tekst `"hello"` i `"HELLO,"` różnią się wielkością liter. Aby to zmienić, możemy wykorzystać kombinację tych dwóch metod:

```Rust
let text = "HELLO, WORLD!";
let new_text = text.to_lowercase().replace("hello", "cześć");
println!("{}", new_text);

// Output: cześć, WORLD!
```

Teraz tekst zostanie poprawnie zamieniony, bez względu na wielkość liter.

## Deep Dive

Dzięki bibliotece standardowej języka Rust, możemy dokonać zamiany tekstu w sposób wygodny i prosty. Warto jednak pamiętać, że biblioteka ta nie jest przeznaczona do zaawansowanych operacji na tekście i nie jest zalecana do zastosowań, gdzie dużo danych wymaga szybkiego przetwarzania.

Jeśli potrzebujemy bardziej zaawansowanych funkcji, warto zwrócić uwagę na biblioteki takie jak `regex` lub `strsim`, które zawierają dodatkowe metody do manipulacji tek