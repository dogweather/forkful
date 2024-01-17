---
title:                "Łączenie ciągów znaków"
html_title:           "Bash: Łączenie ciągów znaków"
simple_title:         "Łączenie ciągów znaków"
programming_language: "Bash"
category:             "Bash"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/bash/concatenating-strings.md"
---

{{< edit_this_page >}}

## Co to jest konkatynacja i dlaczego jest ważna?

Konkatynacja stringów to połączenie kilku ciągów znaków w jeden większy ciąg. Programiści wykorzystują tę technikę, aby tworzyć bardziej złożone i dynamiczne teksty, które są potrzebne w wielu aplikacjach.

## Jak to zrobić?

Aby skonkatynować stringi w Bashu, możemy użyć operatora "+" lub odwrotnego ukośnika "\" do połączenia kilku ciągów znaków w jeden. Przykładowy kod wyglądałby tak:

```Bash
string1="Hello"
string2="world!"
concatenated="$string1 $string2"
echo $concatenated
```

Wyjściem powyższego kodu będzie "Hello world!".

## Głębszy zanurzenie

Konkatynacja stringów jest powszechnie stosowana w wielu językach programowania, a jej historia sięga początków informatyki. Alternatywną metodą może być użycie funkcji "printf" lub "sprintf", które posiadają więcej opcji formatowania tekstu. W Bashu, używając operatora "+", stringi są konwertowane na liczby i następnie dodawane razem, co może być problematyczne przy niektórych typach danych. Dlatego też, lepszym wyborem jest użycie operatora "\".

## Zobacz także

Jeśli chcesz dowiedzieć się więcej o konkatynacji stringów w Bashu, możesz zajrzeć do dokumentacji Bash Guide for Beginners (http://tldp.org/LDP/Bash-Beginners-Guide/html/index.html) lub przejrzeć różne przykłady kodu na stronie Bash Hackers Wiki (https://wiki.bash-hackers.org/).