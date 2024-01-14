---
title:    "Javascript: Uppercasing ciągu znaków"
keywords: ["Javascript"]
---

{{< edit_this_page >}}

## Dlaczego

Czy kiedykolwiek zastanawiałeś się, dlaczego warto zapisać swoją nazwę użytkownika z dużej litery lub wyświetlać tytuł strony z użyciem kapitalizacji? Jest to znacznie łatwiejsze dla ludzi do odczytania i może po prostu wyglądać lepiej. W tym wpisie dowiesz się, jak zastosować kapitalizację w swoim kodzie Javascript.

## Jak to zrobić

Najprostszym sposobem na kapitalizację jest użycie metody ```toUpperCase``` na stringu. Na przykład:

```Javascript
let imie = 'kuba';
console.log(imie.toUpperCase());
```

Ten kod wyświetli "KUBA" w konsoli. Innym sposobem jest użycie metody ```charAt``` do zmiany pierwszej litery stringa na dużą. Możesz również użyć pętli for lub funkcji ```map``` do kapitalizacji każdej litery w stringu.

Jeśli chcesz zmienić tylko pierwszą literę w stringu, możesz użyć metody ```replace``` w połączeniu z wyrażeniem regularnym. Na przykład:

```Javascript
let zdanie = 'to jest przykładowe zdanie';
let kapitalizowaneZdanie = zdanie.replace(/^./, zdanie[0].toUpperCase());
```

W tym przypadku korzystamy z wyrażenia regularnego, aby wybrać pierwszy znak w stringu i zamienić go na duży za pomocą metody ```toUpperCase```. Jeśli chcesz kapitalizować wszystkie słowa w zdaniu, możesz użyć wyrażenia regularnego w połączeniu z pętlą for.

## Deep Dive

Kapitalizacja jest ważnym aspektem wizualną prezentacją danych, jednak w kodzie może również mieć wpływ na wydajność. Niektóre metody, takie jak ```toUpperCase``` i ```toLowerCase```, mogą być wolniejsze od innych, ponieważ tworzą nowe stringi, zamiast modyfikować istniejące.

Często wystarczy użyć metody ```charAt``` lub ```replace``` do kapitalizacji pojedynczych liter, aby uniknąć niepotrzebnego tworzenia nowych stringów.

Jednym z zastosowań kapitalizacji może być również sprawdzenie poprawnego formatu danych. Jeśli oczekujemy, że użytkownik wpisze imię lub nazwisko zaczynające się od dużej litery, możemy użyć kapitalizacji, aby sprawdzić, czy dane zostały wprowadzone prawidłowo.

## Zobacz też

- [Metody stringów w Javascript](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/String)
- [10 sposobów na kapitalizację w Javascript](https://flaviocopes.com/how-to-uppercase-first-letter-javascript/)
- [Regex do kapitalizacji stringów](https://stackoverflow.com/questions/1026069/how-do-i-make-the-first-letter-of-a-string-uppercase-in-javascript)