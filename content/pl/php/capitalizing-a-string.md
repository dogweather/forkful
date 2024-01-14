---
title:                "PHP: Zapisywanie wielkich liter ciągu znaków"
simple_title:         "Zapisywanie wielkich liter ciągu znaków"
programming_language: "PHP"
category:             "PHP"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/php/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## Dlaczego

Często w programowaniu potrzebujemy modyfikować ciągi znaków, na przykład zmiany ich rozmiaru czy stylu. Jeden z takich przypadków to konieczność zamiany pierwszej litery każdego słowa w ciągu na wielką literę. Ten proces nazywa się "kapitalizacją" i jest bardzo częstym zadaniem w PHP. Dlatego w tym artykule pokażemy, jak w prosty sposób kapitalizować ciągi znaków w PHP.

## Jak To Zrobić

Do kapitalizacji ciągów znaków w PHP możemy użyć funkcji `ucwords()`. Przyjmuje ona jeden argument - ciąg znaków, który chcemy zmodyfikować. Następnie zwraca ona nowy ciąg znaków z pierwszą literą każdego słowa kapitalizowaną.

```PHP
$ciag = 'to jest przykladowy ciag do kapitalizacji';
echo ucwords($ciag);
```

**Output:** To Jest Przykladowy Ciag Do Kapitalizacji

Funkcja `ucwords()` jest szczególnie przydatna, gdy chcemy kapitalizować również słowa zaczynające się od znaku specjalnego, na przykład `-` lub `.`.

```PHP
$ciag = 'ciało-energia-pojawienie się';
echo ucwords($ciag);
```

**Output:** Ciało-Energia-Pojawienie Się

## Deep Dive

Jeśli chcemy kapitalizować ciąg znaków bez uwzględniania znaków specjalnych, możemy użyć funkcji `ucfirst()`. Różni się ona od `ucwords()` tym, że kapitalizuje tylko pierwszą literę ciągu.

Funkcja `ucwords()` wykorzystuje również ustalony przez użytkownika separator słów. Domyślnie jest nim spacja, ale można to zmienić poprzez przekazanie opcjonalnego drugiego argumentu do funkcji.

```PHP
$ciag = 'to jest przykladowy ciag do kapitalizacji';
echo ucwords($ciag, "-");
```

**Output:** To Jest Przykladowy Ciag Do Kapitalizacji

W przypadku gdy potrzebujemy kapitalizować tylko pierwsze litery wyrazów, ale zachowując oryginalny rozmiar liter pozostałych znaków, możemy użyć funkcji `mb_convert_case()`, która dostępna jest w PHP w wersjach 7 i wyższych.

## Zobacz Także

- [Dokumentacja funkcji ucwords() w PHP](https://www.php.net/manual/en/function.ucwords.php)
- [Dokumentacja funkcji ucfirst() w PHP](https://www.php.net/manual/en/function.ucfirst.php)
- [Dokumentacja funkcji mb_convert_case() w PHP](https://www.php.net/manual/en/function.mb-convert-case.php)