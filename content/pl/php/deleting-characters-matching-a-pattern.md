---
title:    "PHP: Usuwanie znaków pasujących do wzorca."
keywords: ["PHP"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/pl/php/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## Dlaczego

Czy kiedykolwiek zdarzyło Ci się, że potrzebowałeś usunąć określone znaki z tekstu? Na przykład, gdy musiałeś usunąć wszystkie numery telefonów z dłuższego ciągu tekstu. Właśnie, dlatego należy wiedzieć, jak usunąć znaki odpowiadające określonemu wzorcowi w języku PHP. W tym artykule pokażemy Ci, dlaczego i jak to zrobić.

## Jak to zrobić

```PHP
$text = 'Ten tekst zawiera różne znaki, takie jak #, %, ?, a także numery telefonów: 123-456-789 i 987-654-321. Potrzebujemy usunąć wszystkie numery telefonów z tego tekstu.';
$pattern = '/\d{3}-\d{3}-\d{3}/'; // wzorzec odpowiadający numerom telefonów

$clean_text = preg_replace($pattern, '', $text); // używając funkcji preg_replace, usuwamy wszystkie znalezione dopasowania wzorca
echo $clean_text;
```

```PHP
Oto oczekiwany wynik:

Ten tekst zawiera różne znaki, takie jak #, %, ?, a także numery telefonów:  i . Potrzebujemy usunąć wszystkie numery telefonów z tego tekstu.
```

## Deep Dive

Funkcja `preg_replace` jest użytecznym narzędziem w języku PHP do usuwania znaków odpowiadających danemu wzorcowi. Pozwala nam na szybkie i sprawnie usunięcie niepożądanych znaków z tekstu. W naszym przykładzie użyliśmy prostego wzorca `/ \d{3}-\d{3}-\d{3} /` odpowiadającego numerom telefonów w formacie XXX-XXX-XXX. Jednakże, `preg_replace` może również przyjmować bardziej złożone wzorce, co daje nam większe możliwości w usuwaniu znaków z tekstu.

## Zobacz także

- [Dokumentacja PHP: funkcja `preg_replace`](https://www.php.net/manual/en/function.preg-replace.php)
- [Wyrażenia regularne w języku PHP](https://www.php.net/manual/en/reference.pcre.pattern.syntax.php)
- [Tutorial wideo: Usuwanie znaków z tekstu w języku PHP](https://www.youtube.com/watch?v=BC5NcqhUUbc)