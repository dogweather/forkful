---
title:    "Gleam: Tworzenie pliku tekstowego"
keywords: ["Gleam"]
---

{{< edit_this_page >}}

## Dlaczego

Pisanie plików tekstowych jest jedną z podstawowych czynności w programowaniu. Jest to proces, który pozwala na zapisywanie i odczytywanie danych w sposób uporządkowany, co jest niezbędne w wielu projektach. Bez umiejętności pisania plików tekstowych, programiści mogliby mieć trudności w zachowaniu i przetwarzaniu informacji.

## Jak to zrobić

Gleam jest nowym językiem programowania, który stawia sobie za zadanie uprościć proces tworzenia aplikacji w języku Elm. Jedną z jego zalet jest wbudowany moduł FileSystem, który umożliwia operacje na plikach. Aby napisać plik tekstowy w Gleam, wykonaj poniższe kroki:

<p align="center">
    <img src="https://unsplash.com/photos/1DM7XKY62tM" alt="Kod odczytujący i piszący do pliku tekstowego">
</p>

```
Gleam> fs::open_file("dane.txt", fs::WriteOnly) |> fs::write("To jest testowy tekst.") |> fs::close()
```

Output:
```
Ok
```

Powyższy kod tworzy nowy plik o nazwie "dane.txt" i zapisuje w nim podany tekst. Plik zostaje automatycznie zamknięty po wykonaniu operacji.

## Głębszy zanurzenie

Oprócz podstawowych operacji, moduł FileSystem oferuje również wiele innych funkcji, takich jak odczytywanie zawartości pliku, zmienianie nazwy i usuwanie plików. Aby dowiedzieć się więcej o możliwościach tego modułu, warto przeczytać dokumentację Gleama lub eksperymentować z różnymi funkcjami.

## Zobacz również

- [Dokumentacja modułu FileSystem w języku Gleam](https://gleam.run/modules/fs.html)
- [Przykłady kodów z wykorzystaniem modułu FileSystem w języku Gleam](https://github.com/gleam-lang/gleam/tree/master/examples/filesystem)

Dzięki umiejętności pisania plików tekstowych w języku Gleam, będziesz mógł w prosty sposób zarządzać danymi w swoich projektach. Bądź na bieżąco z najnowszymi funkcjami i możliwościami tego języka, aby jeszcze bardziej ułatwić sobie pracę.