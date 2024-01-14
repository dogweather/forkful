---
title:    "Fish Shell: Wyszukiwanie i zamiana tekstu"
keywords: ["Fish Shell"]
---

{{< edit_this_page >}}

## Dlaczego

Często podczas programowania, potrzebujemy zmienić określone ciągi tekstowe w naszym kodzie. Może to wynikać z błędu w pisowni, zmiany nazwy zmiennej lub po prostu z uaktualnienia naszego projektu. W takich sytuacjach bardzo przydatnym narzędziem jest przeszukiwanie i zamiana tekstu. Pozwala ono szybko i skutecznie wprowadzić zmiany w naszym kodzie.

## Jak to zrobić

Fish Shell posiada wbudowany mechanizm do przeszukiwania i zamiany tekstu, który jest bardzo łatwy w użyciu. Aby to zrobić, należy użyć następującej składni:

```Fish Shell
sed -i 's/stary_tekst/nowy_tekst/g' plik.txt
```

W powyższym przykładzie, zdefiniowaliśmy stary tekst, który chcemy znaleźć i zamienić oraz nowy tekst, który chcemy wstawić. Przy użyciu opcji "-i" modyfikujemy nasz plik bezpośrednio, zamiast tylko wyświetlać zmienioną wersję na ekranie. Opcja "g" oznacza, że chcemy przeszukać cały plik, a nie tylko pierwsze wystąpienie.

## Deep Dive

Istnieje wiele różnych opcji w narzędziu "sed", które pozwalają na precyzyjne przeszukiwanie i zamianę tekstu. Możemy na przykład dodawać warunki, w których zamiana tekstu ma wystąpić, lub wybrać tylko część pliku do modyfikacji. Możemy również używać wyrażeń regularnych, aby jeszcze bardziej dopasować nasze wyszukiwanie.

Ponadto, w Fish Shell istnieje również możliwość użycia polecenia "replace", które jest jeszcze prostsze w użyciu. Przy użyciu tego polecenia możemy zmienić pojedyncze wystąpienie tekstu, wszystkie wystąpienia w danym wierszu lub wszystkie wystąpienia w całym pliku.

Jeśli chcemy dowiedzieć się więcej o przeszukiwaniu i zamianie tekstu w Fish Shell, warto przeczytać oficjalną dokumentację lub zapoznać się z wyrażeniami regularnymi.

## Zobacz także

- Oficjalna dokumentacja Fish Shell na temat przeszukiwania i zamiany tekstu: https://fishshell.com/docs/current/cmds/sed.html
- Przykłady użycia polecenia "replace" w Fish Shell: https://fishshell.com/docs/current/cmds/replace.html
- Wprowadzenie do wyrażeń regularnych: https://www.w3schools.com/jsref/jsref_obj_regexp.asp