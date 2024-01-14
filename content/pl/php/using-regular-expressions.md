---
title:                "PHP: Używanie wyrażeń regularnych"
programming_language: "PHP"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/php/using-regular-expressions.md"
---

{{< edit_this_page >}}

## Dlaczego
Regularne wyrażenia są niezbędnym narzędziem w każdym języku programowania, a w PHP jest to szczególnie pomocne. Pozwalają na wyszukiwanie i manipulację tekstem, co jest niezwykle przydatne w wielu projektach. Dzięki nim można szybko i sprawnie przetwarzać duże ilości danych, co znacznie ułatwia pracę programistom.

## Jak to zrobić
Aby zacząć korzystać z regularnych wyrażeń w PHP, należy użyć funkcji `preg_match` lub `preg_replace`, które pozwalają na porównywanie wzorca z tekstem lub zamianę wybranego fragmentu tekstu na inny. Przykładowy kod wykorzystujący funkcję `preg_match` wygląda następująco:

```PHP
$text = "Ten tekst zawiera numer telefonu: 123-456-789." 
if (preg_match("/[0-9]{3}-[0-9]{3}-[0-9]{3}/", $text, $matches)){
    echo "Numer telefonu: " . $matches[0];
}
```

Wynik działania powyższego kodu to:
`Numer telefonu: 123-456-789`

Można również wykorzystać wyrażenia regularne do wykonywania bardziej zaawansowanych operacji, takich jak wyodrębnianie fragmentów tekstu lub zmiana jego formatu. Przykładowy kod z wykorzystaniem funkcji `preg_replace` wygląda następująco:

```PHP
$text = "Ten tekst będzie zmieniony na duże litery."
$text = preg_replace("/tekst/", "napis", $text);
echo strtoupper($text);
```

Wynik działania powyższego kodu to:
`TEN NAPIS BĘDZIE ZMIENIONY NA DUŻE LITERY.`

## Głębsza analiza
W PHP istnieje wiele opcji i funkcji związanych z wyrażeniami regularnymi, ale najważniejszą rzeczą do zapamiętania jest to, że wzorzec musi być dokładnie dopasowany do tekstu, aby zadziałał. Wyrażenia regularne są również wrażliwe na wielkość liter, więc należy uważnie dopasowywać wzorzec, aby uzyskać pożądany efekt.

Pamiętaj również, że w PHP istnieją różne symbole, które można użyć przy tworzeniu wzorca, na przykład `+` oznaczające, że można dopasować jeden lub więcej wystąpień danego znaku, lub `?` oznaczający, że dany znak jest opcjonalny. Dzięki temu można tworzyć bardzo szczegółowe i precyzyjne wzorce.

## Zobacz również
Jeśli chcesz dowiedzieć się więcej o wyrażeniach regularnych w PHP, polecam przeczytać następujące artykuły:

- [Oficjalna dokumentacja PHP o wyrażeniach regularnych](https://www.php.net/manual/en/ref.pcre.php)
- [Tutorial na temat wyrażeń regularnych w PHP](https://www.tutorialspoint.com/php/php_regular_expression.htm)
- [Poradnik od PHP Academy](https://phpacademy.org/courses/regular-expressions-in-php)