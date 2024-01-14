---
title:                "Fish Shell: Konwertowanie daty na łańcuch znaków."
programming_language: "Fish Shell"
category:             "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/fish-shell/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## Dlaczego

Każdy programista wie, że praca z danymi czasowymi jest nieodłącznym elementem wielu projektów. Często konieczne jest konwertowanie daty na ciąg znaków, aby móc wyświetlić ją czy zapisać w odpowiednim formacie. W tym artykule opiszemy jak przekonwertować datę w języku Fish Shell oraz podzielimy się cennymi wskazówkami, które ułatwią Ci pracę z tym zadaniem.

## Jak to zrobić

Do przekonwertowania daty na ciąg znaków w języku Fish Shell możemy wykorzystać funkcję `date` oraz operator `strftime`. Przykładowy kod wyglądałby następująco:

```Fish Shell
set my_date (date)
set my_string $my_date.strftime("%d/%m/%Y")
echo $my_string
```

W powyższym przykładzie używamy funkcji `date`, aby pobrać aktualną datę i zapisujemy ją w zmiennej `my_date`. Następnie korzystając z operatora `strftime` przekonwertowujemy tę datę na ciąg znaków w wybranym przez nas formacie. W ostatnim kroku wyświetlamy ten ciąg za pomocą funkcji `echo`.

Poniżej znajduje się lista kilku przydatnych formatów, które możemy wykorzystać przy konwersji daty na ciąg znaków:

- `%d` - dzień miesiąca w formacie dziesiętnym (np. 01, 02, 03)
- `%m` - miesiąc w formacie dziesiętnym (np. 01, 02, 03)
- `%Y` - rok w formacie czterocyfrowym (np. 2021)
- `%H` - godzina w formacie 24-godzinnym (np. 13, 14, 15)
- `%M` - minuta w formacie dziesiętnym (np. 01, 02, 03)
- `%S` - sekunda w formacie dziesiętnym (np. 01, 02, 03)

## Deep Dive

Funkcja `date` w języku Fish Shell może przyjąć również inne parametry, które pozwolą nam dostosować sposób wyświetlania daty. Dokładną dokumentację tych parametrów można znaleźć w oficjalnej dokumentacji Fish Shell.

Warto również zwrócić uwagę, że operator `strftime` jest dostępny nie tylko przy konwertowaniu daty, ale również przy konwertowaniu innych wartości, takich jak np. czas ostatniej modyfikacji pliku. Możliwości wykorzystania tego operatora są więc bardzo szerokie.

## Zobacz również

- Dokumentacja devdocs.io
- Oficjalna dokumentacja Fish Shell
- Artykuł na blogu Fish Shell o funkcji `date` i operatorze `strftime` (w języku angielskim)