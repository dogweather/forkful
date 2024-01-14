---
title:    "PHP: Zapis wielkimi literami łańcucha znaków"
keywords: ["PHP"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/pl/php/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## Dlaczego

W dzisiejszych czasach programowanie stało się nieodłączną częścią naszego codziennego życia. Od tworzenia aplikacji mobilnych po obsługę systemów internetowych, trudno jest znaleźć dziedzinę, która nie jest zależna od kodowania. Dzięki temu powstaje również potrzeba znajomości języków programowania, takich jak PHP, które pozwalają na tworzenie dynamicznych i funkcjonalnych stron internetowych.

Jedną z najważniejszych umiejętności w programowaniu jest praca z ciągami znaków. W niektórych sytuacjach może być konieczne przekształcenie ciągu znaków do postaci, w której każde słowo zaczyna się wielką literą. Jest to przydatne, na przykład, w przypadku nazw własnych lub tytułów, gdzie zgodnie ze stylistyką języka polskiego każde słowo powinno być zapisane wielką literą.

## Jak to zrobić

Aby przekształcić ciąg znaków na postać złożoną z wyłącznie wielkich liter, wystarczy skorzystać z wbudowanej funkcji w języku PHP - `strtoupper()`. Wystarczy podać jako argument funkcji żądany ciąg znaków i zostanie on zwrócony w postaci składającej się z samych wielkich liter.

Przykładowy kod wyglądałby następująco:

```PHP
$string = "programowanie PHP jest fascynujące!";
$upper_string = strtoupper($string);
echo $upper_string; // OUTPUT: PROGRAMOWANIE PHP JEST FASCYNŨJĄCE!
```

Widzimy, że wszystkie litery zostały przekształcone na wielkie, co może być przydatne w wielu przypadkach programistycznych.

## Głębsza analiza

Funkcja `strtoupper()` jest tylko jednym z przykładów, w jaki sposób można przekształcać ciągi znaków w języku PHP. Istnieje również funkcja `ucwords()`, która przekształca pierwszą literę każdego słowa w podanym tekście na wielką, oraz `ucfirst()`, która przekształca jedynie pierwszą literę całego tekstu na wielką.

Warto również pamiętać, że funkcje te uwzględniają ustawienia lokalizacyjne danego systemu operacyjnego. Dzięki temu można zachować odpowiednie formatowanie dla różnych języków.

## Zobacz także

#### Manual PHP - `strtoupper()` 
https://www.php.net/manual/en/function.strtoupper.php

#### Manual PHP - `ucwords()` 
https://www.php.net/manual/en/function.ucwords.php

#### Manual PHP - `ucfirst()` 
https://www.php.net/manual/en/function.ucfirst.php