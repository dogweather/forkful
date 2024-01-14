---
title:    "PHP: Szukanie i zastępowanie tekstu"
keywords: ["PHP"]
---

{{< edit_this_page >}}

## Dlaczego

W dzisiejszych czasach, kiedy prawie wszystko jest dostępne online, wykorzystanie języka programowania PHP staje się coraz popularniejsze. Ten język umożliwia nam tworzenie dynamicznych stron internetowych w łatwy sposób. Jednym z wielu przydatnych narzędzi, które PHP oferuje, jest funkcja wyszukiwania i zamiany tekstu.

## Jak to zrobić

Kiedy tworzymy stronę internetową, często mamy do czynienia z dużą ilością tekstu. Czasami chcemy wprowadzić pewne zmiany w tekście, np. zmienić słowo, usunąć lub zamienić fragment. Dzięki funkcji wyszukiwania i zamiany teksty w języku PHP, możemy to zrobić bardzo szybko i bez problemów.

Aby wyszukać i zamienić tekst w PHP, musimy użyć funkcji `str_replace()`. Przyjmuje ona trzy argumenty: szukaną frazę, frazę zastępującą oraz tekst, w którym chcemy dokonać zmian. W poniższym przykładzie zamienimy słowo "Hello" na "Cześć" w zdaniu "Hello World!".

```PHP
<?php
    echo str_replace("Hello", "Cześć", "Hello World!");
?>
```

Wynik:

```PHP
Cześć World!
```

Ponadto, jeśli chcemy wyszukać i zmienić wiele różnych fraz, możemy użyć pętli `foreach` i tablicy z danymi. W poniższym przykładzie zamienimy wszystkie wystąpienia francuskich słów na angielskie.

```PHP
<?php
    $replace = array("Pomme" => "Apple", "Fraise" => "Strawberry", "Orange" => "Orange");
    $sentence = "J'aime manger une Pomme chaque jour. Je préfère les fraises aux oranges.";
    echo str_replace(array_keys($replace), $replace, $sentence);
?>
```

Wynik:

```PHP
J'aime manger une Apple chaque jour. Je préfère les Strawberries aux Oranges.
```

## Deep Dive

Funkcja `str_replace()` nie musi służyć tylko do zwykłych zamian słów. Możemy również użyć jej do dokonywania bardziej złożonych zmian, np. zmiany koloru tekstu lub formatowania daty. W poniższym przykładzie zmienimy kolor słowa "tekst" na czerwony i odwrócimy kolejność wyświetlania daty.

```PHP
<?php
    $text = "Ten tekst jest niebieski a data to 30-06-2021.";
    $replace = array("niebieski" => "<span style='color:red'>czerwony</span>", "-" => "/");
    echo str_replace(array_keys($replace), $replace, $text);
?>
```

Wynik:

```PHP
Ten tekst jest <span style='color:red'>czerwony</span> a data to 2021/06/30.
```

## Zobacz także

Jeśli chcesz dowiedzieć się więcej o funkcji `str_replace()` i innych przydatnych narzędziach PHP, możesz odwiedzić poniższe linki:

1. [Oficjalna dokumentacja PHP](https://www.php.net/manual/en/function.str-replace.php)
2. [Samouczek w języku polskim na w3schools](https://www.w3schools.com/php/func_string_str_replace.asp)
3. [Poradnik na Tutsplus](https://code.tutsplus.com/tutorials/you-cant-live-without-the-str_replace-function--net-9048)