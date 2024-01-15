---
title:                "Konwertowanie daty na ciąg znaków"
html_title:           "Fish Shell: Konwertowanie daty na ciąg znaków"
simple_title:         "Konwertowanie daty na ciąg znaków"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/fish-shell/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## Dlaczego

Każdy programista, który pracuje z datami, zostanie z pewnością zmuszony do konwertowania ich na ciągi znaków w różnych formatach. To ważny aspekt w programowaniu, aby zapewnić poprawną prezentację dat w interfejsie użytkownika lub w plikach.

## Jak to zrobić

Konwersja daty na ciąg znaków wymaga użycia odpowiedniej funkcji w języku programowania. W przypadku Fish Shell, używamy funkcji `date` z opcją `+%Y-%m-%d` do konwersji na format rok-miesiąc-dzień.

Przykładowy kod w Fish Shell wyglądałby następująco:

```
set dzis=date +%Y-%m-%d
echo $dzis  
```

Po uruchomieniu tego kodu, otrzymamy aktualną datę w formacie roku-miesiąc-dzień:

```
2021-11-10
```

## Wnikliwe zagłębienie

W przypadku, gdy potrzebujemy bardziej szczegółowej konwersji daty, możemy użyć opcji `date` z odpowiednimi znacznikami. Na przykład, używając opcji `+%A, %B %d, %Y` otrzymamy datę w formacie dzisiejszy dzień tygodnia, nazwa miesiąca, dzień i rok.

```
set dzis=date +%A, %B %d, %Y
echo $dzis  
```

To spowoduje wyświetlenie daty w następującym formacie:

```
środa, listopad 10, 2021
```

Za pomocą opcji `date` można również zmienić strefę czasową, na przykład używając `+%z` i `+%Z` otrzymamy informacje o przesunięciu czasowym oraz pełną nazwę strefy czasowej.

```
set dzis=date +%A, %B %d, %Y +%z %Z
echo $dzis  
```

Output będzie zawierał dodatkowe informacje, takie jak "Central European Time" lub "UTC+0100".

## Zobacz również

- [Dokumentacja Fish Shell: Date Manipulation](https://fishshell.com/docs/current/cmds/date.html)
- [Strona Fish Shell w języku polskim](https://fishshell.com/docs/current/index.html#polish)