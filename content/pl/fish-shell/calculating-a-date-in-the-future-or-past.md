---
title:                "Fish Shell: Obliczanie daty w przyszłości lub przeszłości"
programming_language: "Fish Shell"
category:             "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/fish-shell/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## Dlaczego

Obliczanie dat w przyszłości lub przeszłości jest nieodłączną częścią programowania. Warto więc poznać narzędzia, które ułatwią nam ten proces i pozwolą oszczędzić czas. W tym artykule opiszemy, jak przy pomocy języka programowania Fish Shell możemy wykonywać takie obliczenia.

## Jak wykonać obliczenia daty w przyszłości lub przeszłości przy użyciu Fish Shell

Fish Shell jest językiem programowania, który pozwala na wygodne i szybkie wykonywanie obliczeń daty. W poniższych przykładach użyjemy wbudowanej funkcji `date` oraz operatorów arytmetycznych, aby pokazać, jak można obliczyć daty w przyszłości lub przeszłości.

### Obliczanie daty w przyszłości

Aby obliczyć datę, na którą wydarzenie odbędzie się np. za miesiąc, możemy użyć poniższego kodu:

```Fish Shell
date -j -v+1m
```

Wykorzystujemy tutaj opcję `-v` (z ang. "value") wraz z wartością `+1m`, co oznacza "dodaj 1 miesiąc do obecnej daty". Możemy również wykorzystać operator `+`, np. `+1d` oznacza dodanie jednego dnia, `+2w` - dodanie dwóch tygodni, itp.

### Obliczanie daty w przeszłości

Jeśli chcemy obliczyć datę, na którą wydarzenie już się odbyło, np. tydzień temu, możemy użyć tego samego kodu, tylko z użyciem operatora `-` zamiast `+`:

```Fish Shell
date -j -v-1w
```

Możemy również wykorzystać więcej niż jeden operator, np. `+2w5d` oznacza dodanie dwóch tygodni i pięciu dni.

## Głębsza analiza

Obliczanie daty w przyszłości lub przeszłości może uwzględniać różne czynniki, takie jak liczba dni w miesiącu, rok przestępny itp. W zależności od potrzeb, możemy dodać do naszego kodu warunki, które uwzględnią te przypadki.

Na przykład, aby dodać jedną miesiąc do obecnej daty, ale uwzględnić także liczbę dni w miesiącu, możemy użyć poniższego kodu:

```Fish Shell
if test (date -j +%m) = 2
    date -j -v+1m -v-1d
else if test (date -j +%d) -gt 28
    date -j -v+1m -v-3d
else
    date -j -v+1m
end
```

W ten sposób nasz program będzie uwzględniał różne ustawienia daty i zawsze będzie zwracać poprawny wynik.

## Zobacz także

- Dokumentacja Fish Shell - https://fishshell.com/docs/current/index.html
- Poradnik Fish Shell dla początkujących - https://dev.to/gboduljak/introduction-to-fish-shell-3lon
- Przełączanie między katalogami w Fish Shell - https://www.ostechnix.com/cd-command-fish-shell-switching-between-directories/