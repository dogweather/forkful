---
title:                "PHP: Wyodrębnianie podciągów"
simple_title:         "Wyodrębnianie podciągów"
programming_language: "PHP"
category:             "PHP"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/php/extracting-substrings.md"
---

{{< edit_this_page >}}

## Dlaczego

Często, podczas pisania kodu PHP, możemy natknąć się na potrzebę wyodrębnienia części tekstu z większego ciągu znaków. Jest to niezbędne, gdy chcemy wyciągnąć informacje ze złożonych danych lub zmodyfikować tekst na określony sposób. W tym artykule dowiesz się, jak wykorzystać funkcję substr() do ekstrakcji podciągów i jak może Ci to ułatwić programowanie w PHP.

## Jak to zrobić

Funkcja substr() jest wbudowana w PHP i służy do wyodrębniania podciągów ze stringów. Aby jej użyć, musimy podać dwa argumenty - pierwszym jest string, z którego chcemy wyciągnąć podciąg, a drugim jest pozycja początkowa oraz (opcjonalnie) długość podciągu. Zapoznajmy się z przykładem:

```PHP
$tekst = "Witaj w świecie PHP!";
echo substr($tekst, 0, 5);
```

W wyniku otrzymamy wyświetlony fragment tekstu "Witaj". Pierwszy argument wskazuje na zmienną $text, drugi określa, że chcemy wyciąć fragment od pierwszego znaku (o indeksie 0) i na długość 5 znaków. Jeśli nie podamy trzeciego argumentu, zostanie wyświetlony podciąg od podanej pozycji do końca stringa. Spróbujmy teraz wyodrębnić inny fragment:

```PHP
$tekst = "Lubisz programować w PHP?";
echo substr($tekst, 15);
```

Wynikiem będzie wyświetlenie zapytania "PHP?". W tym przypadku pominięliśmy trzeci argument, więc zostanie wyświetlony podciąg o długości od 15 znaku do końca stringa. Możemy również wykorzystać liczbę ujemną jako drugi argument, co oznacza liczenie od końca stringa - np. -5 oznacza, że podciąg będzie składał się z ostatnich 5 znaków. Poniższy przykład wyświetli podciąg "lować":

```PHP
$tekst = "Programowanie w PHP jest fajne!";
echo substr($tekst, -6);
```

## Deep Dive

Funkcja substr() pozwala także na dokładniejsze określenie pozycji rozpoczęcia podciągu oraz jego długości. Możemy również wykorzystać ją w pętlach lub innych warunkach, aby dynamicznie wyodrębniać różne fragmenty tekstu. Warto zapoznać się ze wszystkimi możliwościami tej funkcji, ponieważ może ona być bardzo pomocna w pracy z tekstem w PHP.

## Zobacz także

- Dokumentacja PHP dla funkcji substr() - (https://www.php.net/manual/en/function.substr.php)
- Przykłady użycia substr() na stronie W3Schools - (https://www.w3schools.com/php/func_string_substr.asp)
- Współpracujące z substr() funkcje do pracy z tekstem w PHP - (https://www.php.net/manual/en/ref.strings.php)