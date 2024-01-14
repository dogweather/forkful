---
title:    "PHP: Wydobywanie podciągów"
keywords: ["PHP"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/pl/php/extracting-substrings.md"
---

{{< edit_this_page >}}

## Dlaczego

Czy kiedykolwiek zdarzyło Ci się potrzebować jedynie części tekstu z ciągu znaków? Na przykład, gdy chcesz wyświetlić tylko imię z pełnego nazwiska lub numer telefonu bez kodu kraju? W takich sytuacjach przydatna może okazać się funkcja wyciągania podciągów (substrings) w języku PHP. Pozwala ona na pobranie określonej liczby znaków z istniejącego ciągu i wykorzystanie ich w celach programistycznych lub wyświetlenia dla użytkownika. W tym krótkim artykule opowiemy o tym, jak wykorzystać tę funkcjonalność w praktyce.

## Jak to zrobić

Funkcja substr() w języku PHP pozwala na wyciągnięcie podciągu z istniejącego ciągu znaków. Jej składnia jest następująca:

```PHP
substr(tekst, od którego znaku pobrać, ile znaków pobrać);
```

Przykładowo, jeśli chcemy wyciągnąć imię z nazwiska "Kowalski", możemy użyć funkcji w ten sposób:

```PHP
$nazwisko = "Kowalski";
$imie = substr($nazwisko, 0, 5);
echo $imie; //wynik: Kowal
```

W tym przypadku funkcja pobierze pierwsze pięć znaków z ciągu "Kowalski", czyli "Kowal" i przypisze je do zmiennej $imie. Następnie, dzięki poleceniu echo, wyświetlimy jej zawartość na ekranie.

Inną przydatną opcją jest możliwość liczenia znaków od końca ciągu wstecz. Używając ujemnego indeksu, możemy wyciągnąć ostatnie znaki. Przykładowo, jeśli chcemy wyświetlić tylko ostatnie trzy znaki z nazwiska, możemy użyć takiej składni:

```PHP
$nazwisko = "Kowalski";
$wyciagniete = substr($nazwisko, -3);
echo $wyciagniete; //wynik: ski
```

W ten sposób pobierzemy ostatnie trzy znaki z ciągu "Kowalski" i wyświetlimy je za pomocą polecenia echo.

## Wnikliwa analiza

Funkcja substr() może być również wykorzystana w bardziej złożonych przypadkach. Na przykład, gdy mamy do czynienia z danymi w formacie daty i chcemy wyciągnąć tylko dzień lub miesiąc. W takim przypadku możemy użyć ujemnego indeksu i kombinacji funkcji substr() i strlen().

```PHP
$data = "2020-10-15";
$dzien = substr($data, -2);
$miesiac = substr($data, 5, 2);
echo $dzien; //wynik: 15
echo $miesiac; //wynik: 10
```

W powyższym przykładzie najpierw pobieramy ostatnie dwa znaki z całego ciągu, co daje nam liczbę reprezentującą dzień. Następnie, za pomocą funkcji substr() pobieramy dwa znaki od piątego od końca indeksu, co odpowiada za miesiąc. Wykorzystując tę kombinację z funkcją strlen() możemy dowolnie dostosowywać ilość pobieranych znaków i uzyskać potrzebne nam informacje.

## Zobacz też

- [Dokumentacja PHP - funkcja substr()](https://www.php.net/manual/en/function.substr.php)
- [Tutorial wideo: Wprowadzenie do funkcji substr() w języku PHP](https://www.youtube.com/watch?v=1ZhLzWmNJtA)
- [Przykładowe zadania wykor