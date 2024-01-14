---
title:                "Ruby: Konwersja daty na ciąg znaków"
programming_language: "Ruby"
category:             "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/ruby/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## Dlaczego

Konwersja daty na ciąg znaków jest częstym wyzwaniem dla programistów Ruby. W tym artykule dowiesz się, dlaczego tak jest i jak możesz sprawnie przekształcić datę w ciąg znaków w swoim kodzie.

## Jak to zrobić

Konwersja daty na ciąg znaków jest możliwa dzięki użyciu metody `strftime`, która przekształca datę na formatowany ciąg znaków. Przykładowy kod wykorzystujący tę metodę wygląda następująco:

```Ruby
Time.now.strftime("%d/%m/%Y")
```

Powstały ciąg znaków będzie składł się z liczb reprezentujących dzień, miesiąc i rok, oddzielonych ukośnikami.

Można również użyć innych symboli formatujących, na przykład `%B` dla nazwy miesiąca lub `%A` dla nazwy dnia tygodnia. Więcej symboli dostępnych jest w dokumentacji Rubiego.

Poniżej przedstawiamy przykładowy kod oraz wynik dla daty 1 lipca 2021 roku:

```Ruby
Time.new(2021, 7, 1).strftime("%d/%m/%Y")
```

Wynik: `01/07/2021`

## Zagłębienie w temat

Aby lepiej zrozumieć, jak działa konwersja daty na ciąg znaków, warto poznać strukturę daty w Rubym. Obiekt `Time` reprezentuje datę i czas w Rubym, a metoda `strftime` służy do formatowania tego obiektu.

Przykładowo, jeśli chcesz wyświetlić godzinę, możesz skorzystać z symbolu `%H`, czyli 24-godzinnego zapisu. Dzięki temu kod `Time.now.strftime("%H:%M:%S")` zwróci aktualną godzinę, minutę i sekundę.

Konwertując datę na ciąg znaków, musisz pamiętać o odpowiednim ustawieniu strefy czasowej. W przeciwnym razie może dojść do nieprawidłowego wyświetlenia daty.

## Zobacz również

- Dokumentacja Rubiego dotycząca działania metody `strftime`: https://ruby-doc.org/core-3.0.1/Time.html#method-i-strftime
- Przykładowe formaty i symbole dla metody `strftime`: https://devhints.io/datetime
- Artykuł na blogu Kamilahodjaeva.com o konwersji daty na ciąg znaków: https://kamilahodjaeva.com/blog/tips-and-tricks-datetime-formatting-in-ruby/