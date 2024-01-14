---
title:                "Gleam: Pisanie testów"
simple_title:         "Pisanie testów"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/gleam/writing-tests.md"
---

{{< edit_this_page >}}

## Dlaczego pisanie testów jest ważne?

Pisanie testów jest nieodłączną częścią procesu programowania i pomaga nam zapewnić, że nasz kod działa poprawnie. Poprzez pisanie testów, jesteśmy w stanie wykryć błędy i problemy w naszym kodzie jeszcze przed jego przetestowaniem przez użytkowników. Jest to nie tylko efektywny sposób na poprawianie błędów, ale także pomaga nam utrzymać nasz kod w dobrej jakości.

## Jak pisać testy w Gleam?

Dzięki językowi programowania Gleam i jego zestawowi wbudowanych funkcji testerskich, pisanie testów jest proste i przyjemne. Poniżej przedstawimy kilka przykładów testów napisanych w Gleam oraz oczekiwane wyjście dla każdego z nich.

```Gleam
test "Dodawanie dwóch liczb powinno zwrócić prawidłowy wynik" {
    let wynik = 2 + 3
    expect(wynik) |> to_equal(5)
}

test "Dzielenie przez zero powinno zwrócić błąd" {
    let wynik = 4 / 0
    expect(wynik) |> to_equal(Error)
}
```

W powyższych przykładach mamy testuje dodawanie dwóch liczb oraz dzielenie przez zero. Dzięki użyciu funkcji `expect` i operatora `|>`, możemy wyrazić nasze oczekiwania co do wyników naszych obliczeń. Jeśli test jest zaliczony, otrzymamy informację o tym, że test przebiegł pomyślnie. Jeśli jednak test jest niezaliczony, otrzymamy informację o błędzie oraz szczegóły, które pomogą nam znaleźć i naprawić błąd.

## Wnikliwsze spojrzenie na pisanie testów

Pisanie testów jest nie tylko kwestią pokrycia kodu testami, ale także wykorzystania odpowiednich technik i strategii. Dzięki temu możemy stworzyć bardziej skuteczne i kompleksowe testy, które zapewnią nam pewność co do działania naszego kodu. Poniżej przedstawiamy kilka przydatnych artykułów na temat pisanie testów w Gleam:

- [Dokumentacja Gleam o pisaniu testów](https://gleam.run/book/tour/tests.html)
- [Blog Gleam: "Pięć sposobów na udane testowanie w Gleam"](https://gleam.run/blog/testing.html)
- [Artykuł "Test Driven Development w Gleam"](https://mokscy.name/test-driven-development-with-gleam/)

## Zobacz także

- [Dokumentacja Gleam](https://gleam.run/)
- [Blog Gleam](https://gleam.run/blog/)
- [Repozytorium Gleam na GitHubie](https://github.com/gleam-lang/gleam)

Dzięki tym wskazówkom i narzędziom, pisanie testów w Gleam będzie prostsze i skuteczniejsze, co pozwoli nam uniknąć błędów i utrzymać nasz kod w dobrej jakości. Zachęcamy do korzystania z testów w trakcie procesu pisania kodu i dzięki temu budować lepsze i bardziej niezawodne aplikacje.