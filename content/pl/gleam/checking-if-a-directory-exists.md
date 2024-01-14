---
title:                "Gleam: Sprawdzanie istnienia katalogu"
programming_language: "Gleam"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/gleam/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## Dlaczego

Sprawdzanie czy istnieje katalog jest ważną częścią procesu programowania, ponieważ pozwala nam upewnić się, że nasz kod będzie działał poprawnie, bez żadnych błędów związanych z nieistniejącymi katalogami.

## Jak To Zrobić

Aby sprawdzić czy dany katalog istnieje w języku Gleam, możemy skorzystać z funkcji `File.exists?/1` i przekazać jej ścieżkę do katalogu jako argument. Przykładowy kod wyglądałby następująco:

```Gleam
import File

let directory = "sciezka/do/katalogu"

if File.exists?(directory) {
    IO.println("Katalog istnieje")
} else {
    IO.println("Katalog nie istnieje")
}
```

Po uruchomieniu tego kodu, jeśli katalog istnieje, zostanie wyświetlony komunikat "Katalog istnieje", a jeśli nie istnieje - komunikat "Katalog nie istnieje".

## Głębsze Spostrzeżenia

Sprawdzanie istnienia katalogu jest ważne nie tylko ze względu na poprawne działanie programu, ale również ze względu na bezpieczeństwo danych. Przykładowo, jeśli nasz program jest odpowiedzialny za przechowywanie i zarządzanie plikami użytkowników, to ważne jest, aby upewnić się, że nie istnieją przypadkowe katalogi, które mogłyby naruszyć prywatność danych.

Możemy również wykorzystać dodatkowe funkcje, takie jak `File.dir_entries/1`, aby uzyskać listę plików i katalogów znajdujących się w danym katalogu, i odpowiednio nimi zarządzać w naszym kodzie.

## Zobacz Również

- [Dokumentacja języka Gleam dotycząca funkcji File](https://gleam.run/docs/standard-library/file)
- [Poradnik: Tworzenie i zarządzanie katalogami w języku Gleam](https://gleam.run/posts/directory-management.html)
- [Przykładowy kod z funkcją `File.exists?/1`](https://gist.github.com/user/repo/gist-ID)