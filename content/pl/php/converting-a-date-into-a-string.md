---
title:    "PHP: Konwertowanie daty na ciąg znaków"
keywords: ["PHP"]
---

{{< edit_this_page >}}

## Dlaczego

Konwertowanie daty na łańcuch znaków jest ważną częścią programowania, szczególnie w przypadku tworzenia aplikacji internetowych lub systemów do zarządzania danymi. Jest to niezbędne do prawidłowej prezentacji daty dla użytkowników oraz wykonania operacji na danych czasowych.

## Jak to zrobić

```PHP
<?php
$date = "2021-05-15";
$format = "d-m-Y";
$date_string = date($format, strtotime($date));
echo $date_string;
?>
```

W tym krótkim przykładzie uwzględniliśmy datę w formacie "YYYY-MM-DD" i pożądany format wyjściowy "DD-MM-YYYY". Za pomocą funkcji `date()` i `strtotime()` możemy przekonwertować datę na odpowiedni format i przypisać ją do zmiennej `date_string`. Wykorzystując komendę `echo` możemy wyświetlić przekonwertowaną datę w formie łańcucha znaków.

**Output:** 15-05-2021

Możemy również wykorzystać funkcję `str_replace()` do zmiany znaków separatorów daty lub wykorzystać funkcję `date_create()` do konwersji daty z obiektu `DateTime`.

## Głębsza analiza

Konwertowanie daty na łańcuch znaków wymaga użycia funkcji `date()` wraz z jednym z kilku argumentów formatowania. Przedrostek `d`, `m` i `Y` oznaczają odpowiednio dzień, miesiąc i rok. Możemy również wykorzystać inne opcje takie jak `l` dla pełnej nazwy dnia tygodnia, `F` dla pełnej nazwy miesiąca lub `s` dla sekund w formacie dziesiętnym.

Funkcja `strtotime()` jest bardzo przydatna w zarządzaniu datami i czasem w PHP. Przyjmuje ona łańcuch znaków reprezentujący datę i zwraca wartość liczbową, którą można następnie wykorzystać w funkcji `date()`.

## Zobacz również

- [Dokumentacja PHP: date()](https://www.php.net/manual/en/function.date.php)
- [Dokumentacja PHP: strtotime()](https://www.php.net/manual/en/function.strtotime.php)
- [Poradnik wideo: Jak konwertować daty w PHP](https://www.youtube.com/watch?v=d6rC9IwRfKE)