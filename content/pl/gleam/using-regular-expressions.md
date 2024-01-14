---
title:    "Gleam: Używanie wyrażeń regularnych"
keywords: ["Gleam"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/pl/gleam/using-regular-expressions.md"
---

{{< edit_this_page >}}

## Dlaczego

Regularne wyrażenia są niezbędnym narzędziem w każdym programistycznym rękawie. Pozwalają nam na dokładne i szybkie przeszukiwanie tekstu oraz manipulowanie nim. Bez nich trudne byłoby radzenie sobie z złożonymi wyrażeniami, takimi jak adresy email czy numery telefonów. Dzięki nim możemy dokonać wielu skomplikowanych operacji w krótkim czasie, co jest szczególnie ważne w dzisiejszym szybkim świcie technologii.

## Jak to zrobić

Kiedy piszesz w języku Gleam, aby wykorzystać regularne wyrażenia musisz skorzystać z biblioteki o nazwie `regex`. Pierwszym krokiem jest jej zaimportowanie do Twojego kodu:

```Gleam
import regex
```

Następnie możesz użyć funkcji `regex.replace` aby zastosować wyrażenie regularne do danego tekstu. Przykładowo, jeśli chcesz zamienić wszystkie spacje na myślniki w ciągu znaków "Hello World", możesz to zrobić w ten sposób:

```Gleam
let result = regex.replace("Hello World", "\\s", "-")
```

W powyższym przykładzie, pierwszy argument funkcji `replace` to ciąg znaków, w którym chcemy przeprowadzić zmiany. Drugi argument to szukane wyrażenie regularne, a trzeci to wartość, na którą chcemy je zamienić. W tym przypadku chcemy zamienić każdą spację (`\\s`) na myślnik.

## Głębszy zanurzenie

Aby wykorzystać regularne wyrażenia w języku Gleam, musisz mieć pewną znajomość notacji i składni. Regularne wyrażenia są sekwencją znaków, które opisują wzorzec do dopasowania. Mogą zawierać litery, cyfry i znaki specjalne, aby określić specyficzne typy znaków lub sekwencje, które chcemy znaleźć lub zamienić.

Na przykład, wyrażenie regularne `/^\d{3}-\d{3}-\d{4}$/` będzie dopasowane do numeru telefonu w formacie XXX-XXX-XXXX (gdzie "X" oznacza cyfrę). W tym przypadku, `^` oznacza początek wyrażenia, `\d` oznacza cyfrę, a `{3}` mówi, że szukamy trzech cyfr. Znak `-` określa, że po trzech cyfrach musi wystąpić myślnik, a `$` oznacza koniec wyrażenia.

Istnieje wiele różnych kombinacji znaków i sekwencji, które możesz stosować w wyrażeniach regularnych. Dlatego warto poświęcić trochę czasu na naukę ich działania, aby z łatwością korzystać z nich w swoim kodzie.

## Zobacz także

- Dokumentacja biblioteki Regex dla języka Gleam: [https://gleam.run/modules/regex/latest/](https://gleam.run/modules/regex/latest/)
- Przewodnik dla początkujących dotyczący wyrażeń regularnych: [https://regexone.com/](https://regexone.com/)
- Interaktywne narzędzie do testowania wyrażeń regularnych: [https://regex101.com/](https://regex101.com/)