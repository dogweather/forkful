---
title:                "Usuwanie znaków odpowiadających wzorcowi"
html_title:           "PHP: Usuwanie znaków odpowiadających wzorcowi"
simple_title:         "Usuwanie znaków odpowiadających wzorcowi"
programming_language: "PHP"
category:             "PHP"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/php/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## Dlaczego usuwać znaki pasujące do wzorca?

Usuwanie znaków pasujących do wzorca jest często wymagane w programowaniu PHP, ponieważ pozwala na szybkie i skuteczne przetwarzanie danych. Może to być przydatne na przykład podczas filtrowania danych, usuwania niepotrzebnych znaków z tekstu lub utworzenia wyrażeń regularnych dla bardziej skomplikowanych operacji.

## Jak to zrobić?

Implementacja usuwania znaków pasujących do wzorca w PHP jest prosta i wygodna dzięki wbudowanej funkcji `preg_replace()`. Poniżej przedstawiamy przykłady użycia tej funkcji w różnych sytuacjach.

Usuwanie białych znaków z tekstu:
```PHP
$text = "  Tekst   z    białymi    znakami  ";
$text = preg_replace('/\s+/', '', $text);
echo $text;
// Output: "Tekstzbiałymiznakami"
```

Usuwanie wszystkich cyfr z tekstu:
```PHP
$text = "1a2b3c4d";
$text = preg_replace('/[0-9]/', '', $text);
echo $text;
// Output: "abcd"
```

Podmiana wybranego wzorca:
```PHP
$text = "To jest tekst <span>z elementem HTML</span>.";
$text = preg_replace('/<span>(.*?)<\/span>/', 'Zastąpione', $text);
echo $text;
// Output: "To jest tekst Zastąpione."
```

## Deep Dive

Funkcja `preg_replace()` przyjmuje trzy argumenty - wzorzec, zastępujący tekst oraz tekst, na którym wykonujemy operację. Wzorzec jest wyrażeniem regularnym, zgodnie z którym zostaną znalezione pasujące fragmenty tekstu. Zastępujący tekst może być ciągiem znaków lub funkcją, która zwróci wartość zamiany. Spliter wyrażenia `$` użyty do zastępowania elementów HTML to często stosowany wzorzec, ponieważ pozwala zachować oryginalne tagi.

## Zobacz też

- [Dokumentacja funkcji `preg_replace()` w PHP](https://www.php.net/manual/en/function.preg-replace.php)
- [Poradnik programowania w PHP dla początkujących](https://www.php.net/manual/en/tutorial.php)
- [Przykłady wyrażeń regularnych w PHP](https://www.php.net/manual/en/reference.pcre.pattern.syntax.php)