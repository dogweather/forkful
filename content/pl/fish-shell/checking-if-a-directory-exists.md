---
title:                "Sprawdzanie istnienia katalogu"
html_title:           "Fish Shell: Sprawdzanie istnienia katalogu"
simple_title:         "Sprawdzanie istnienia katalogu"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/fish-shell/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

Czym jest sprawdzanie istnienia katalogu i dlaczego programiści to robią?

Sprawdzanie, czy katalog istnieje, jest podstawowym zadaniem, które programiści wykonują, aby upewnić się, że ich program działa poprawnie. Katalog jest po prostu miejscem, w którym można przechowywać pliki w systemie operacyjnym, dlatego jest ważne, aby móc sprawdzić, czy jest on dostępny i można w nim wykonywać różne operacje.

Jak to zrobić w Fish Shell?

Fish Shell jest potężnym narzędziem, które udostępnia nam wiele komend i funkcji, w tym także możliwość sprawdzenia, czy katalog istnieje. Wystarczy użyć kilku prostych poleceń, aby to zrobić. Oto kilka przykładów:

```
Fish Shell: if test -d ~/folder
  echo "Katalog istnieje"
else
  echo "Katalog nie istnieje"
end
```

W tym przykładzie korzystamy z komendy `test`, która służy do sprawdzania warunków w skrypcie shell. Z użyciem opcji `-d` mówimy, że chcemy sprawdzić, czy wskazany katalog istnieje. Wewnątrz instrukcji warunkowej, w zależności od wyniku, wyświetlamy odpowiedni komunikat.

Za pomocą tej samej komendy można także wykonać inne działania, na przykład sprawdzić, czy katalog jest pusty:
```
Fish Shell: if test -d ~/folder && test -z (ls -A ~/folder)
  echo "Katalog jest pusty"
else
  echo "Katalog nie jest pusty"
end
```

Można także wyświetlić listę plików znajdujących się w katalogu, jeśli istnieje:
```
Fish Shell: if test -d ~/folder
  ls ~/folder
else
  echo "Katalog nie istnieje"
end
```

Głębsze zanurzenie

Sprawdzanie istnienia katalogu jest powszechną czynnością w programowaniu i używane jest w wielu językach programowania. W wielu systemach operacyjnych istnieje specjalna funkcja lub komenda służąca do tej operacji, jednak w Fish Shell możemy wykorzystać komendę `test`, która jest również bogata w inne opcje i warunki.

Alternatywnie, istnieje także kilka innych sposobów na sprawdzenie, czy katalog istnieje. Można na przykład wykorzystać polecenie `ls`, które po prostu wyświetla zawartość danego katalogu. Jeśli katalog nie istnieje, pojawi się błąd.

Można także wykorzystać wbudowaną w system funkcję `stat`, która zwraca informacje o danym pliku lub katalogu. Jeśli katalog istnieje, będzie widoczny w wynikach.

Podsumowując, istnieje wiele sposobów na sprawdzenie, czy katalog istnieje w Fish Shell, jednak komenda `test` jest najbardziej przejrzystym i uniwersalnym rozwiązaniem.

Zobacz także

Jeśli chciałbyś dowiedzieć się więcej o Fish Shell i jego funkcjonalności, możesz odwiedzić oficjalną dokumentację na stronie https://fishshell.com/docs oraz zapoznać się z innymi dostępnymi funkcjami i komendami. Możesz także zobaczyć różne przykłady użycia na stronie https://github.com/fish-shell/fish-shell.