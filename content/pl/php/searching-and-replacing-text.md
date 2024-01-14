---
title:                "PHP: Wyszukiwanie i zastępowanie tekstu"
programming_language: "PHP"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/php/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## Dlaczego

Każdy programista lubi mieć czysty i czytelny kod. Czasami jednak musimy zmienić pewne części tekstu w naszym kodzie, aby poprawić wydajność lub dostosować go do zmieniających się wymagań. W takich przypadkach żmudne ręczne zmienianie tekstu może okazać się nieefektywne oraz podatne na błędy. Właśnie wtedy przydaje się umiejętność szybkiego wyszukiwania i zamieniania tekstu w kodzie, co pozwala nam zaoszczędzić czas i zachować spójność w naszym projekcie.

## Jak to zrobić

Do wyszukiwania i zamieniania tekstu w PHP możemy wykorzystać funkcję `str_replace()`. W przykładzie poniżej użyjemy tej funkcji, aby zmienić wszystkie wystąpienia słowa "hello" na "cześć" w naszym tekście.

```PHP
$tekst = "hello, world!";
$nowy_tekst = str_replace("hello", "cześć", $tekst);
echo $nowy_tekst; // wypisze "cześć, world!"
```

Jak widać, funkcja `str_replace()` przyjmuje trzy argumenty - szukany tekst, nowy tekst oraz tekst, w którym chcemy dokonać zmiany. W ten sposób możemy łatwo zmieniać dowolne fragmenty tekstu w zadanym dokumencie.

## Głębszy zanurzenie

Funkcja `str_replace()` posiada również opcję podmiany wielu fragmentów tekstu jednocześnie. Możemy przekazać jej tablicę szukanych i nowych tekstów, co pozwala na zamianę wielu wyrazów na raz. Przykład zastosowania:

```PHP
$tekst = "witaj, świecie!";
$szukaj = array("witaj", "świecie");
$nowy = array("cześć", "świecie");
$nowy_tekst = str_replace($szukaj, $nowy, $tekst);
echo $nowy_tekst; // wypisze "cześć, world!"
```

Dodatkowo możemy wykorzystać funkcję `str_ireplace()` w celu wykonania wyszukiwania i zamiany tekstu, z uwzględnieniem wielkości liter.

## Zobacz też

- [Oficjalna dokumentacja PHP dla funkcji str_replace()](https://www.php.net/manual/en/function.str-replace.php)
- [Funkcje string w PHP - artykuł na blogu programistycznym](https://blogprogramisty.net/funkcje-string-w-php/)
- [Poradnik dla początkujących w PHP - pisanie skryptów](https://developer.mozilla.org/pl/docs/Web/Javascript/Dzisiejszy_JavaScript_podstawy/Zmienne_i_rozdzialki/Skrypty_w_PHP)