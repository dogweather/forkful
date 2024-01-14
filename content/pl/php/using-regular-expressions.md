---
title:    "PHP: Użycie wyrażeń regularnych"
keywords: ["PHP"]
---

{{< edit_this_page >}}

## Dlaczego korzystamy z wyrażeń regularnych?

Wyrażenia regularne są niezwykle przydatnym narzędziem w programowaniu PHP. Służą one do przetwarzania i manipulowania tekstem w sposób bardziej wyrafinowany niż standardowe funkcje. W poniższym wpisie przedstawimy, dlaczego warto poznać i wykorzystywać wyrażenia regularne w swoim kodzie.

## Jak to zrobić?

Kodowanie przykładów i wyników zostanie przedstawiony w blokach ```PHP ... ```. Rozpoczniemy od prostej struktury wyrażenia regularnego, a następnie przedstawimy bardziej zaawansowane przykłady. Na początek, spróbujemy dopasować słowo "kot" w pewnym tekście za pomocą funkcji ```preg_match```.

```PHP
$text = "Ten tekst zawiera słowo kot.";
$pattern = "/kot/";
if (preg_match($pattern, $text)) {
    echo "Słowo 'kot' zostało znalezione w tekście!";
} else {
    echo "Nie znaleziono słowa 'kot' w tekście.";
}

// Wynik: Słowo 'kot' zostało znalezione w tekście!
```

Następnie spróbujemy zamienić wszystkie wystąpienia słowa "kot" na słowo "pies" za pomocą funkcji ```preg_replace```.

```PHP
$text = "Ten tekst zawiera słowo kot.";
$pattern = "/kot/";
$replacement = "pies";
$newText = preg_replace($pattern, $replacement, $text);
echo $newText;

// Wynik: Ten tekst zawiera słowo pies.
```

Oczywiście, wyrażenia regularne są znacznie bardziej wszechstronne i mogą wykonywać wiele różnych operacji na tekście. Zachęcamy do eksperymentowania i poznawania różnych funkcji z dokumentacji PHP.

## Deep Dive

Wyrażenia regularne to potężne narzędzie, ale mogą być również skomplikowane dla początkujących programistów. Warto zauważyć, że wyrażenia regularne są zgodne z wyrażeniami regularnymi używanymi w innych językach programowania, takich jak JavaScript czy Python. Dzięki temu, możesz wykorzystać swoją wiedzę i umiejętności również w innych technologiach.

Podczas używania wyrażeń regularnych, warto pamiętać o zagrożeniu tzw. "rekurencyjnych wyrażeń regularnych". Oznacza to, że wyrażenie regularne może być nieskończone i nigdy nie zakończy się wykonywać, co może doprowadzić do awarii serwera lub przekroczenia limitów zasobów. Należy więc uważnie tworzyć swoje wyrażenia regularne i unikać wykorzystywania rekurencji.

## Zobacz także

- [Dokumentacja PHP - Wyrażenia regularne](https://www.php.net/manual/en/ref.regex.php)
- [10 przydatnych wyrażeń regularnych w PHP](https://medium.com/@anil_singh/useful-regular-expression-regular-interval-to-manage-string-in-php-12c01abf9af)
- [Interaktywne wyrażenia regularne do nauki](https://regexr.com/)
- [Poradnik wyrażeń regularnych dla początkujących](https://www.cheatography.com/davechild/cheat-sheets/regular-expressions/)