---
title:    "Gleam: Czytanie pliku tekstowego"
keywords: ["Gleam"]
---

{{< edit_this_page >}}

## Dlaczego

Czy wiesz, że Gleam jest językiem programowania stworzonym z myślą o bezpieczeństwie i wydajności? Jeśli jesteś zainteresowany/a nauką tego języka, miejmy nadzieję, że nasz wpis pomoże Ci poszerzyć Twoją wiedzę o funkcji odczytu plików tekstowych!

## Jak To Zrobić

Gleam oferuje nam wiele różnych możliwości związanych z obsługą plików tekstowych. Jedną z najpopularniejszych i najprostszych jest funkcja `File.read_text`, która pozwala na odczytanie zawartości tekstu z dowolnego pliku. Możesz wywołać ją w następujący sposób:

```Gleam
let file = File.read_text("./plik.txt")
```

Wynikiem tej funkcji będzie wartość typu `Result` zawierająca zawartość pliku w postaci `Ok(text)` lub `Error(message)` w przypadku błędu.

Możesz również użyć funkcji `File.read_text_to_string` do odczytania pliku bezpośrednio do zmiennej typu `String`. Przykładowy kod wyglądałby tak:

```Gleam
let text = File.read_text_to_string("./plik.txt")
```

Pamiętaj, aby zawsze sprawdzać zwracaną wartość, ponieważ możesz nie mieć dostępu do pliku lub wystąpił inny błąd.

## Pogłębione Wprowadzenie

Funkcje opisane powyżej to tylko podstawowe sposoby odczytywania plików tekstowych w Gleam. Istnieją również inne funkcje, takie jak `File.read`, `File.read_lines` czy `File.read_until`, które pozwalają na bardziej zaawansowane operacje, na przykład odczytywanie pliku w poszczególnych liniach zamiast całej zawartości na raz.

Dodatkowo, warto zaznajomić się z różnymi trybami otwierania plików, takimi jak `read`, `write` czy `append`, które pozwalają na dostosowanie sposobu odczytu do naszych potrzeb.

Jeśli chcesz dowiedzieć się więcej na temat odczytywania plików tekstowych w Gleam, zalecamy zapoznanie się z oficjalną dokumentacją tego języka.

## Zobacz Również

* [Oficjalna Dokumentacja Gleam](https://gleam.run/documentation/)
* [Repozytorium GitHub Gleam](https://github.com/gleam-lang/gleam)