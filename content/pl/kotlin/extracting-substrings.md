---
title:                "Ekstrakcja podciągów"
html_title:           "Kotlin: Ekstrakcja podciągów"
simple_title:         "Ekstrakcja podciągów"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/kotlin/extracting-substrings.md"
---

{{< edit_this_page >}}

# Dlaczego
Często, w czasie pisania kodu w języku Kotlin, może pojawić się potrzeba wyciągnięcia fragmentu tekstu z łańcucha znaków. Może to być na przykład odcięcie prefixu lub sufiksu, lub też wyodrębnienie określonych znaków na podstawie pozycji w łańcuchu. W takich sytuacjach wykorzystanie funkcji do wycinania podłańcuchów jest niezbędną umiejętnością.

# Jak to zrobić
```Kotlin
val text = "To jest przykładowy tekst"
val substring = text.substring(3, 9)

println(substring) // "jest p"
```
Funkcja `substring` pozwala nam na podanie indeksów, od których do których chcemy wyodrębnić fragment tekstu. W powyższym przykładzie, parametry 3 i 9 oznaczają odpowiednio początkowy i końcowy indeks wycinanego podłańcucha. Należy pamiętać, że indeksy są numerowane od zera i nie są wliczane w wyodrębniony podłańcuch.

Funkcja ta posiada również opcjonalny parametr `step`, który określa co który znak chcemy wyodrębnić. Jest to przydatne w przypadku, gdy potrzebujemy wyodrębnić co drugi lub co trzeci znak z tekstu.

```Kotlin
val text = "To jest przykładowy tekst"
val substring = text.substring(3, 9, 2)

println(substring) // "jtprk"
```

# Wnikliwiej
W języku Kotlin istnieje kilka funkcji do wycinania podłańcuchów, w tym `substring()`, `subSequence()` oraz `take()`. Korzystając z metod `substring()` oraz `subSequence()` możemy wyodrębnić fragment tekstu na podstawie indeksów. Natomiast funkcja `take()` pozwala na wyodrębnienie określonej liczby znaków od początku tekstu.

Funkcje te są nie tylko użyteczne w odcięciu fragmentów tekstu, ale mogą również znacznie ułatwić manipulację łańcuchami znaków w naszym kodzie. Dzięki nim możemy na przykład rozbić długi łańcuch na mniejsze fragmenty i łatwiej operować na każdej części osobno.

# Zobacz także
- Dokumentacja języka Kotlin: https://kotlinlang.org/docs/reference/strings.html#string-slices
- Wideo tutorial na temat wycinania podłańcuchów w Kotlinie: https://www.youtube.com/watch?v=U2vhDTY-gTg