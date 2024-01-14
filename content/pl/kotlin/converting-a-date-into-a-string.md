---
title:                "Kotlin: Konwertowanie daty na ciąg znaków"
programming_language: "Kotlin"
category:             "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/kotlin/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## Dlaczego

Kiedy pracujesz z danymi, często istnieje potrzeba konwertowania jednego typu danych na inny. Jedną z tych konwersji jest zamiana daty na ciąg znaków. W tym artykule dowiesz się, jak to zrobić w języku Kotlin i dlaczego jest to ważne.

## Jak to zrobić

Możesz użyć funkcji `format()` w klasie `SimpleDateFormat` do konwersji daty na ciąg znaków. Przykładowy kod wyglądałby następująco:

```Kotlin
val date = Date()
val format = SimpleDateFormat("dd/MM/yyyy")
val dateString = format.format(date)
println(dateString)
```

W powyższym przykładzie, najpierw tworzony jest obiekt `Date`, który przechowuje aktualną datę. Następnie tworzony jest format daty i używany do skonwertowania obiektu `Date` na ciąg znaków. Wynikiem jest ciąg znaków reprezentujący aktualną datę w formacie "dd/MM/yyyy". Po wywołaniu funkcji `println()` na zmiennej `dateString`, wyświetlony zostanie taki wynik: "29/04/2022".

## Deep Dive

Teraz, gdy wiesz już, jak skonwertować datę na ciąg znaków, warto dowiedzieć się więcej na temat formatowania. Istnieje wiele różnych wzorców formatowania, które możesz użyć do określenia wyglądu końcowego ciągu znaków. Na przykład, jeśli chcesz wyświetlić rok jako dwie cyfry, możesz użyć formatu "dd/MM/yy" zamiast "dd/MM/yyyy".

Warto również pamiętać, że klasa `SimpleDateFormat` jest obiektowo orientowana, co oznacza, że możesz zachowywać różne formaty dla różnych obiektów daty. Możesz również tworzyć własne wzorce formatowania, dostosowując je do swoich potrzeb.

## Zobacz również

- [Dokumentacja języka Kotlin](https://kotlinlang.org/docs/working-with-dates.html)
- [Poradnik konwersji dat w języku Kotlin](https://medium.com/@sagar0497/date-formatting-in-kotlin-75f6d94fe296)
- [Przykładowy kod konwersji daty na ciąg znaków w języku Kotlin](https://gist.github.com/alephZa/d05e3af7a758d0cbe2b570bdcda562ba)