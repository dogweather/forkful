---
title:                "Pobieranie aktualnej daty"
html_title:           "Fish Shell: Pobieranie aktualnej daty"
simple_title:         "Pobieranie aktualnej daty"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/fish-shell/getting-the-current-date.md"
---

{{< edit_this_page >}}

## Co & Dlaczego?
Pobranie aktualnej daty to podstawowa czynność w programowaniu, umożliwiająca śledzenie czasu wykonania operacji i tworzenie datowych formatów wyjściowych. Dzięki temu programiści mogą precyzyjnie kontrolować czas oraz dostosować go do swoich potrzeb.

## Jak to zrobić:
Korzystając z powłoki Fish możemy wyświetlić aktualną datę za pomocą jednej z wbudowanych funkcji. Wystarczy wpisać polecenie ```date```, aby uzyskać datę i godzinę w formacie domyślnym. Jeśli chcielibyśmy dostosować wyjście do określonych wymagań, możemy skorzystać z opcji. Na przykład, aby wyświetlić tylko dzień tygodnia i miesiąca, możemy użyć polecenia ```date '+%a %b'```, a wynik będzie miał postać ```Thu Mar```.

## Pogłębiony zanurzenie:
Pobranie aktualnej daty jest wykonywane przez wiele systemów operacyjnych, dlatego też istnieje wiele różnych sposobów na jego wykonanie. Jedną z popularniejszych opcji jest wykorzystanie wbudowanej funkcji ```date```, jednak istnieją także inne narzędzia i biblioteki takie jak Moment.js czy strftime, które oferują bardziej rozbudowane możliwości formatowania daty i godziny.

## Zobacz także:
Jeśli chcesz dowiedzieć się więcej o tym, jak działa pobranie aktualnej daty w Fish Shell lub zobaczyć przykładowe kody, możesz skorzystać z poniższych źródeł:
- Dokumentacja Fish Shell: https://fishshell.com/docs/current/cmds/date.html
- Oficjalna strona Moment.js: https://momentjs.com/
- Biblioteka strftime: https://strftime.org/