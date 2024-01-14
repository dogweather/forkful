---
title:    "PHP: Pobieranie aktualnej daty"
keywords: ["PHP"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/pl/php/getting-the-current-date.md"
---

{{< edit_this_page >}}

## Dlaczego

Dlaczego powinniśmy wykorzystać bieżącą datę w programowaniu? Aktualna data jest ważnym elementem w wielu projektach, zwłaszcza w aplikacjach internetowych. Pomaga nam śledzić czas wydarzeń, zarządzać czasowymi interwałami i wiele więcej. W tym wpisie dowiesz się jak w prosty sposób wykorzystać bieżącą datę w PHP.

## Jak to zrobić

W PHP mamy dostęp do wielu funkcji, które pozwalają nam na pracę z datami. Jedną z podstawowych metod jest użycie funkcji `date()`. Poniżej możesz zobaczyć przykładowe wywołanie tej funkcji:

```PHP
echo date("d-m-Y");
```

Wynik tego kodu będzie wyglądał tak: `11-10-2021`. Jak widzisz, funkcja ta zwraca nam aktualną datę w formacie `dzien-miesiac-rok`. Możemy również wykorzystać argumenty tej funkcji, aby uzyskać inną formę daty. Na przykład, jeśli chcemy otrzymać dokładną godzinę, możemy użyć następującego kodu:

```PHP
echo date("H:i:s");
```

Wynik tego kodu będzie wyglądał tak: `11:34:26`. Jak widzisz, używając różnych argumentów, możesz otrzymać dokładnie to, czego potrzebujesz.

## Głębsze zanurzenie

Funkcja `date()` to tylko jedna z wielu metod, które możemy wykorzystać w PHP do pracy z datami. Istnieje również funkcja `strtotime()`, która pozwala nam na operacje na datach, np. dodawanie lub odejmowanie dni, miesięcy czy lat od bieżącej daty. Możesz również użyć gotowych klas, takich jak `DateTime`, aby jeszcze bardziej rozbudować swoje umiejętności w pracy z datami.

## Zobacz również

- [Dokumentacja PHP - Data i czas](https://www.php.net/manual/en/datetime.html)
- [10 przydatnych funkcji PHP do pracy z datami](https://www.codepolitan.com/10-przydatnych-funkcji-php-do-pracy-z-datami)
- [Praca z datami w PHP](https://egzamininformatyczny.pl/praca-z-danymi-w-php/)