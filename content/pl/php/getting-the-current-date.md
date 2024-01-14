---
title:    "PHP: Pobieranie aktualnej daty"
keywords: ["PHP"]
---

{{< edit_this_page >}}

## Dlaczego?

Każdego dnia wykorzystujemy datę w naszym życiu - w spotkaniach, zadaniach, wydarzeniach. W programowaniu nie jest inaczej - daty są niezwykle ważne w tworzeniu aplikacji i stron internetowych. Dlatego w tym artykule pokażę Wam, jak uzyskać bieżącą datę za pomocą PHP i dlaczego warto to umieć.

## Jak to zrobić?

```PHP
$data = date('d-m-Y');
echo $data;
```

Wywołując funkcję `date()` z podaniem formatu daty w parametrze, możemy uzyskać bieżącą datę. W powyższym przykładzie użyliśmy formatu `d-m-Y`, który zwraca datę w formacie dzień-miesiąc-rok. Możemy również użyć innych formatów, np. `m-d-Y`, `Y/m/d`, `d-m-Y H:i:s`, aby uzyskać datę i godzinę.

Warto również wspomnieć, że możemy stosować funkcję `date()` do innych zmiennych, np. `$time = date('H:i:s');`, aby uzyskać bieżącą godzinę.

## Wnikliwiej

Funkcja `date()` wykorzystuje aktualny strefę czasową ustawioną w pliku konfiguracyjnym PHP. Możemy ją zmienić za pomocą funkcji `date_default_timezone_set()`, podając jako parametr nazwę strefy czasowej.

Możemy również użyć funkcji `strtotime()`, aby przekonwertować datę w formacie tekstowym na znacznik czasu, który później możemy wykorzystać do uzyskania innych informacji, np. `date('d-m-Y', strtotime('tomorrow'));` zwróci nam datę jutrzejszą.

## Zobacz także

- [Dokumentacja PHP - funkcja date()](https://www.php.net/manual/en/function.date.php)
- [Dokumentacja PHP - funkcja date_default_timezone_set()](https://www.php.net/manual/en/function.date-default-timezone-set.php)
- [Dokumentacja PHP - funkcja strtotime()](https://www.php.net/manual/en/function.strtotime.php)