---
title:                "Fish Shell: Sprawdzanie czy istnieje katalog"
programming_language: "Fish Shell"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/fish-shell/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## Dlaczego

Cześć! Jeśli jesteś programistą pracującym z językiem Fish Shell, możliwe, że w pewnym momencie będziesz potrzebować sprawdzić, czy dany katalog istnieje. Może to być wymagane do przechowywania plików lub do wykonania pewnych działań tylko wtedy, gdy dany katalog istnieje. W tym wpisie dowiesz się, jak to zrobić.

## Jak to zrobić

Nie ma potrzeby używania zewnętrznych narzędzi lub programów do sprawdzenia istnienia katalogu w Fish Shell. Możemy wykorzystać wbudowane funkcje i zmienne, aby uzyskać tę informację. Oto kilka przykładów kodu:

```
set folder "folder_name"

if test -d $folder
    echo "Katalog istnieje!"
else
    echo "Katalog nie istnieje"
end
```

Powyższy kod przyjmuje nazwę katalogu i wykorzystuje funkcję `test -d` do sprawdzenia, czy jest on katalogiem. Jeśli tak, zostanie wyświetlony komunikat "Katalog istnieje!", w przeciwnym wypadku wyświetlimy informację o nieistniejącym katalogu.

Możesz również użyć zmiennej `status`, która zawiera informacje o ostatniej operacji, aby uzyskać bardziej szczegółowe informacje o wyniku sprawdzenia istnienia katalogu. Przykładowy kod wykorzystujący tę zmienną wyglądałby tak:

```
set folder "folder_name"

test -d $folder
if test $status -eq 0
    echo "Katalog istnieje!"
elif test $status -eq 1
    echo "Katalog nie istnieje!"
else
    echo "Wystąpił błąd podczas sprawdzania istnienia katalogu."
end
```

## Głębszy zanurzenie

Jeśli jesteś ciekawy, jak dokładnie działa funkcja `test -d`, jest to sprawdzane przez sprawdzenie, czy podana ścieżka jest katalogiem i czy użytkownik ma do niego dostęp do odczytu. Jeśli tak, funkcja zwróci wartość 0, w przeciwnym wypadku zwróci wartość 1. Jest to również powód, dla którego możemy użyć zmiennej `status`, aby uzyskać więcej informacji o wyniku sprawdzenia.

## Zobacz również

Jeśli jesteś zainteresowany innymi funkcjami i możliwościami, jakie oferuje Fish Shell, zapraszamy do zapoznania się z poniższymi linkami:

- [Dokumentacja Fish Shell](https://fishshell.com/docs/current/index.html)
- [Poradnik po Fish Shell](https://fishshell.com/docs/current/tutorial.html)
- [Obsługa błędów w Fish Shell](https://fishshell.com/docs/current/tutorial.html#error-handling)

Dzięki za przeczytanie naszego wpisu. Mamy nadzieję, że ten krótki poradnik był pomocny i pomoże Ci w pracy z Fish Shell. Powodzenia!