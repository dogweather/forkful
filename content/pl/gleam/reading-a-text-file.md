---
title:                "Gleam: Odczytywanie pliku tekstowego"
programming_language: "Gleam"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/gleam/reading-a-text-file.md"
---

{{< edit_this_page >}}

## Dlaczego

Czy kiedykolwiek próbowałeś przeczytać duży plik tekstowy i znaleźć konkretne informacje w nim zawarte? Nie jest to łatwe zadanie, szczególnie kiedy plik ten zawiera tysiące linii tekstu. Jednak dzięki programowaniu w języku Gleam, możesz łatwo przeszukiwać pliki tekstowe i wyszukiwać potrzebne informacje. Czytanie plików tekstowych może znacznie ułatwić pracę z danymi i zapewnić szybki dostęp do potrzebnych informacji.

## Jak to zrobić

Aby przeczytać plik tekstowy w języku Gleam, będziemy używać funkcji wbudowanej `File.read`, która przyjmuje jako argumenty nazwę pliku oraz opcjonalny kodowania. Przykładowo, jeśli chcemy odczytać plik tekstowy o nazwie "dane.txt", możemy to zrobić w następujący sposób:

```Gleam
let plik = File.read("dane.txt", :utf8)
```

Wynikiem tej funkcji będzie tuple zawierający status odczytu oraz sam plik tekstowy, który możemy następnie przetworzyć do dalszej analizy. Przykładowo, jeśli w pliku "dane.txt" mamy wpisane imię i nazwisko, możemy je wypisać na ekranie w następujący sposób:

```Gleam
let imie = File.read("dane.txt", :utf8) |> Tuple.set_at(1)
let nazwisko = File.read("dane.txt", :utf8) |> Tuple.set_at(2)

io.print("Imię: ", imie)
io.print("Nazwisko: ", nazwisko)
```

Powyższy kod wyświetli na ekranie "Imię: John" oraz "Nazwisko: Smith", zakładając że w pliku "dane.txt" znajdują się te dane.

## Deep Dive

Podczas czytania pliku tekstowego w języku Gleam, można także wykorzystać funkcję `File.read_line`, która pozwala odczytać pojedynczą linię tekstu z pliku. Dodatkowo, funkcja `File.read_lines` pozwala na odczytanie wszystkich linii tekstu i zwraca je w postaci listy. W ten sposób możemy łatwo przeszukać cały plik i znaleźć potrzebne informacje.

## Zobacz też

- Dokumentacja języka Gleam: [https://gleam.run](https://gleam.run)
- Przetwarzanie plików CSV w języku Gleam: [https://gleam.run/examples/csv.html](https://gleam.run/examples/csv.html)
- Przykłady użycia funkcji `File` w języku Gleam: [https://gleam.run/docs/stdlib/File.html](https://gleam.run/docs/stdlib/File.html)