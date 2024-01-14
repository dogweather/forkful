---
title:    "Fish Shell: Konwersja ciągu znaków na małe litery"
keywords: ["Fish Shell"]
---

{{< edit_this_page >}}

## Dlaczego

Większość programistów doskonale zdaje sobie sprawę, jak ważna jest praca z właściwymi danymi. Często przychodzi nam stykać się ze stringami, czyli sekwencjami znaków, które jednak muszą być odpowiednio sformatowane, aby nasz kod mógł z nich korzystać. Jednym z przydatnych narzędzi w programowaniu jest konwersja stringów na małe litery. Dlaczego warto poznać ten proces? Otóż może okazać się niezbędny w wielu sytuacjach, zwłaszcza przy pracy z danymi użytkowników lub tworzeniu algorytmów sortowania. W tym artykule opowiem Ci o tym, jak w prosty sposób wykonać tę operację w Fish Shell.

## Jak to zrobić

Konwersja stringów na małe litery w Fish Shell jest bardzo prostym procesem. Wystarczy użyć wbudowanej funkcji `lowercase`, która przyjmuje jako argumenty stringi i zwraca je w formacie małych liter. Poniżej przedstawiam przykład kodu oraz jego wynik w terminalu:

```Fish Shell
set string "Przykladowy TEKST"
echo (lowercase $string)
```

Output:

```
przykladowy tekst
```

Jak widać, funkcja `lowercase` zmieniła wszystkie litery w stringu na małe. Warto również zwrócić uwagę, że funkcja ta nie modyfikuje oryginalnego stringu, tylko zwraca nowy. Dzięki temu możemy wciąż korzystać z pierwotnego stringu w dalszej części kodu.

## Głębszy zanurzenie

Warto również wiedzieć, że konwersja na małe litery w Fish Shell jest case-insensitive, czyli nie ma znaczenia, czy używamy dużych czy małych liter przy wywołaniu funkcji. Przykładowo, funkcja `lowercase` będzie działać tak samo w przypadku wyrażenia "TEKST" jak i "tekst". Warto również pamiętać, że to tylko jedna z wielu wbudowanych funkcji związanych z manipulacją stringami w Fish Shell. Jeśli interesuje Cię to zagadnienie, warto zapoznać się również z funkcjami `upper` czy `substr`.

## Zobacz również

* Dokumentacja Fish Shell: https://fishshell.com/docs/current/index.html
* Funkcje do manipulacji stringami w Fish Shell: https://fishshell.com/docs/current/commands.html#string-manipulation-commands
* Wideo tutorial o operacjach na stringach w Fish Shell: https://www.youtube.com/watch?v=Ti3v_NKkg_Y