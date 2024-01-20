---
title:                "Wydobywanie podciągów"
html_title:           "Python: Wydobywanie podciągów"
simple_title:         "Wydobywanie podciągów"
programming_language: "Java"
category:             "Java"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/java/extracting-substrings.md"
---

{{< edit_this_page >}}

## Co i dlaczego?

Wycinanie podciągów (substring) polega na pobieraniu specyficznej części ciągu znaków. Programiści robią to, aby manipulować danymi tekstowymi i tworzyć bardziej dynamiczne programy.

## Jak to zrobić:

W Javie, możemy wykorzystać metodę `substring()`. Oto przykład:
```Java
public class Main {
    public static void main(String[] args) {
        String str = "Hello, World!";
        String subStr = str.substring(7, 12);
        System.out.println(subStr);
    }
}
```
Powyższy kod wydrukuje: `World`.

## Głębszy wgląd:

Historia: Metoda `substring()` istnieje w Javie od początku, co czyni ją jednym ze standardowych narzędzi do manipulacji tekstem. 

Alternatywy: Możemy także korzystać z biblioteki Apache Commons Lang's StringUtils, która posiada metody, takie jak `substringBetween()`.

Implementacja: `substring()` działa na zasadzie indeksowania znaków. Pierwszy indeks jest włączany, ale ostatni jest wyłączany, co oznacza, że `str.substring(7, 12)` dostarczy nam znaki z 7-11 indeksu.

## Zobacz też:

2. [Java String - Extracting More Than One Substring - stackoverflow](https://stackoverflow.com/questions/14536187/java-string-extracting-more-than-one-substring)
3. [StringUtils - Apache Commons Lang](https://commons.apache.org/proper/commons-lang/javadocs/api-release/org/apache/commons/lang3/StringUtils.html)