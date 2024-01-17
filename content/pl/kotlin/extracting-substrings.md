---
title:                "Wydobywanie podciągów"
html_title:           "Kotlin: Wydobywanie podciągów"
simple_title:         "Wydobywanie podciągów"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/kotlin/extracting-substrings.md"
---

{{< edit_this_page >}}

## Co & Dlaczego?

Wyciąganie podciągów to proces wyodrębniania fragmentów tekstu z danego ciągu znaków. Programiści często wykonują tę czynność, aby uzyskać dostęp do konkretnych informacji lub zmodyfikować dany tekst.

## Jak to zrobić:

Poniżej znajdziesz przykłady kodów w języku Kotlin oraz odpowiadające im wyjścia.

```Kotlin 
val text = "Witaj w świecie Kotlin!"
println(text.substring(12)) // wyświetli "Kotlin!"
println(text.substring(12, 15)) // wyświetli "Kot"
```

W powyższym przykładzie używamy metody substring() do wyciągnięcia podciągu tekstu z określonego indeksu lub przedziału indeksów.

## Głębsza analiza:

Wyodrębnianie podciągów jest powszechnie używaną operacją w wielu językach programowania. Powstało wiele różnych sposobów na przeprowadzanie tej czynności, jednak metoda substring() jest jedną z najprostszych i najbardziej popularnych opcji.

Wykorzystując dłuższy przedział indeksów, można również przeprowadzić operację znaną jako "wytnij i wklej", czyli usunięcie danego fragmentu tekstu i wstawienie nowej treści w jego miejsce.

Metoda substring() jest dostępna w wielu językach programowania, a nie tylko w Kotlinie. Jeśli jednak korzystasz z innego języka, być może będzie ona nazywana w nieco inny sposób, np. w Pythonie jest to funkcja slice().

## Zobacz również:

Dla większej głębi wiedzy na temat wyodrębniania podciągów oraz innych przydatnych operacji na tekście, możesz zajrzeć do dokumentacji języka Kotlin lub obejrzeć tutoriale dostępne na YouTube.