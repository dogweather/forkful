---
title:                "Sprawdzanie, czy istnieje katalog"
html_title:           "Fish Shell: Sprawdzanie, czy istnieje katalog"
simple_title:         "Sprawdzanie, czy istnieje katalog"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/fish-shell/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## Dlaczego

Czy kiedykolwiek musiałeś sprawdzić, czy dany katalog istnieje, zanim wykonałeś w nim jakieś operacje? Może jesteś nowym użytkownikiem powłoki Fish i potrzebujesz pomocy ze sprawdzaniem istnienia katalogów? W tym artykule dowiesz się, jak to zrobić i dlaczego jest to ważne.

## Jak to zrobić

Sprawdzenie istnienia katalogu w Fish Shell jest proste i wymaga użycia operatora `test` oraz flagi `-d`. Poniżej przedstawiony jest przykładowy kod, który sprawdza, czy katalog o nazwie "docs" istnieje w bieżącym katalogu:

```Fish Shell
if test -d docs
    echo "Katalog 'docs' istnieje!"
else
    echo "Katalog 'docs' nie istnieje."
end
```

Jeśli katalog "docs" istnieje, w konsoli zostanie wyświetlony komunikat "Katalog 'docs' istnieje!". W przeciwnym razie, zostanie wyświetlony komunikat "Katalog 'docs' nie istnieje."

Możesz również przetestować istnienie dowolnego katalogu, podając jego ścieżkę jako argument w flagi `-d`. Na przykład, aby sprawdzić, czy katalog "projekty" istnieje w bieżącym katalogu, użyjemy następującego kodu:

```Fish Shell
if test -d projects
    echo "Katalog 'projekty' istnieje!"
else
    echo "Katalog 'projekty' nie istnieje."
end
```

## Deep Dive

Głównym powodem, dlaczego warto sprawdzać istnienie katalogów przed wykonywaniem w nich operacji, jest uniknięcie niepotrzebnych błędów i wyjątków. Jeśli na przykład próbujemy dodać plik do nieistniejącego katalogu, otrzymamy błąd, który może zakłócić działanie naszego programu.

Sprawdzając istnienie katalogu przed wykonaniem operacji, możemy ochronić nasz kod przed nieprzewidzianymi błędami. Ponadto, przy sprawdzaniu istnienia katalogu, możemy również wykonać dodatkowe operacje, takie jak informowanie użytkownika o istnieniu lub braku katalogu lub przełączenie do innego katalogu, jeśli aktualny katalog nie istnieje.

## Zobacz też

- Dokumentacja Fish Shell: https://fishshell.com/docs/current/index.html
- Poradnik dla początkujących w Fish Shell: https://fishshell.com/docs/current/tutorial.html
- Kolekcja przydatnych poleceń Fish Shell: https://github.com/fish-shell/fish-shell/blob/master/README.md