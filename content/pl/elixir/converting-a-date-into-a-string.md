---
title:    "Elixir: Konwersja daty na ciąg znaków"
keywords: ["Elixir"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/pl/elixir/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## Dlaczego

Jeśli programujesz w Elixirze i masz potrzebę konwertowania daty na łańcuch znaków, w tym artykule dowiesz się jak to zrobić. Umiejętność ta może przydać się w wielu różnych sytuacjach, na przykład przy tworzeniu raportów lub przy obsłudze dat w interfejsach użytkownika.

## Jak to zrobić

```Elixir
DateTime.to_string(~N[2021-10-20 19:30:00])
```
```
"2021-10-20 19:30:00Z"
```

Możesz skorzystać z wbudowanej funkcji `DateTime.to_string/1`, aby przekonwertować datę na łańcuch znaków. Podaj jedynie datę jako argument, a otrzymasz standardowy format z czasem i strefą czasową. Alternatywnie, możesz także użyć opcjonalnych parametrów funkcji, aby dostosować formatowanie według swoich potrzeb.

```Elixir
NaiveDateTime.to_string(~N[2021-10-20 19:30:00], "{1}. {0} o godzinie {2}:{3}")
```
```
"20.10.2021 o godzinie 19:30"
```

Funkcja `NaiveDateTime.to_string/2` pozwala na większą kontrolę nad formatowaniem daty. Wystarczy podać dwa argumenty - datę oraz format, w którym chcemy otrzymać łańcuch znaków. W przykładzie powyżej użyliśmy znaków specjalnych, takich jak `{0}`, `{1}`, itp., aby wstawiać odpowiednie elementy daty w wybranym przez nas miejscu.

## Deep Dive

W Elixirze, praca z datami jest bardzo wygodna dzięki wbudowanym funkcjom. Ponadto, język ten wspiera również formatowanie liczb, wielokrotnych stref czasowych oraz ułatwia konwersję daty na timestampy. Jeśli chcesz pogłębić swoją wiedzę na ten temat, można zapoznać się z dokumentacją na temat pracy z data i czasem w Elixirze.

## Zobacz też

- [Dokumentacja Elixir na temat pracy z datą i czasem](https://hexdocs.pm/elixir/DateTime.html)
- [Przydatne funkcje do pracy z datą i czasem w Elixirze](https://medium.com/@ulisescabrera/elixir-datetime-functions-19a05ae6aef2)
- [Konwersja daty na timestamp w Elixirze](https://www.opkode.com/blog/working-with-datetime-in-elixir/)