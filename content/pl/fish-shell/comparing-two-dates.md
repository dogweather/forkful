---
title:                "Fish Shell: Porównywanie dwóch dat"
programming_language: "Fish Shell"
category:             "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/fish-shell/comparing-two-dates.md"
---

{{< edit_this_page >}}

## Dlaczego

Porównywanie dwóch dat może być bardzo przydatne w programowaniu, szczególnie gdy chcemy sprawdzić jaki kod został wykonany w danym przedziale czasowym lub w przypadku traktowania dat jako klucza w naszych aplikacjach.

## Jak to zrobić

Jeśli pracujesz w środowisku Fish Shell, masz szczęście! W tej powłoce mamy wiele przydatnych funkcji, które ułatwiają porównywanie dat. Aby porównać dwie daty, możesz skorzystać z polecenia `date` i operatorów porównania, takich jak `>` (większe), `<` (mniejsze) czy `=` (równe).

```Fish Shell

set date1 (date -u +%Y-%m-%d) # ustawienie pierwszej daty jako bieżąca data
set date2 2020-01-01 # ustawienie drugiej daty jako 1 stycznia 2020

if [ $date1 > $date2 ] # porównanie dat za pomocą operatora ">"
    echo "Pierwsza data jest późniejsza niż druga"
else 
    echo "Pierwsza data jest wcześniejsza lub równa drugiej"
end

```

Powyższy kod wyświetli nam odpowiedni komunikat, w zależności od rezultatu porównania. Możemy również wykorzystać inne operatory, aby określić, czy dwie daty są równe lub czy jedna jest późniejsza od drugiej.

## Głębsze zagłębienie

W Fish Shell możemy również wykorzystać funkcję `string to date`, która pozwala konwertować ciągi znaków na daty. W ten sposób możemy porównywać daty, które nie są w formacie daty, ale są przechowywane jako tekst w naszych zmiennych.

```Fish Shell

set date1 (string to date "2020-12-31" "%Y-%m-%d") # konwersja tekstu na datę
set date2 2020-12-31 # ustawienie drugiej daty również jako 31 grudnia 2020

if [ $date1 = $date2 ] # porównanie dat za pomocą operatora "="
    echo "Daty są równe"
else 
    echo "Daty nie są równe"
end

```

Powyższy kod wykorzystuje funkcję `string to date` oraz operator porównania `=` i wyświetli komunikat, który stwierdza, czy dwie daty są równe czy nie.

Należy również pamiętać, że w Fish Shell możemy porównywać również godziny i minuty, jeśli chcemy dokładniej określić przedziały czasowe.

## Zobacz również

Chcesz dowiedzieć się więcej o porównywaniu dat w Fish Shell? Sprawdź poniższe linki:
- [Oficjalna dokumentacja Fish Shell](https://fishshell.com/docs/current/index.html)
- [Porównywanie dat w Fish Shell](https://fishshell.com/docs/current/index.html#comparison-operators)
- [Funkcja string to date w Fish Shell](https://fishshell.com/docs/current/cmds/string-to-date.html)