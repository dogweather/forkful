---
title:                "Używając wyrażeń regularnych"
html_title:           "Java: Używając wyrażeń regularnych"
simple_title:         "Używając wyrażeń regularnych"
programming_language: "Java"
category:             "Java"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/java/using-regular-expressions.md"
---

{{< edit_this_page >}}

## Co i dlaczego?
Wykorzystywanie wyrażeń regularnych to proces, którym programiści mogą szybko i wygodnie przetwarzać tekst, szukając wzorców i wyrażeń w tekście. Jest to często wykorzystywane w celu walidacji danych, weryfikacji adresów email, a także w tworzeniu wyrażeń warunkowych.

## Jak to zrobić:
Wykorzystując klasę Regex w języku Java, można łatwo wykorzystać wyrażenia regularne do wykonywania różnych operacji na tekście. Przykładowe użycie wygląda następująco:
```Java
String text = "Witaj, jestem programistą Java!";
Pattern pattern = Pattern.compile("Java");
Matcher matcher = pattern.matcher(text);
if(matcher.find()){
  System.out.println("Znaleziono wyrażenie: " + matcher.group());
} else {
  System.out.println("Nie znaleziono wyrażenia");
}
```
**Output:**
> Znaleziono wyrażenie: Java

## Głębsze zanurzenie:
Wyrażenia regularne zostały wprowadzone w 1970 roku przez amerykańskiego informatyka Kennetha Thompsona, a obecnie są wykorzystywane w wielu językach programowania, nie tylko w Javie. Alternatywami dla wyrażeń regularnych są m.in. funkcje string w językach programowania lub biblioteki do przetwarzania tekstu. Implementacja wyrażeń regularnych w języku Java jest oparta na silniku języka Perl i dostępna jest w pakiecie java.util.regex.

## Zobacz także:
- Dokumentacja Javy dla klasy Pattern: https://docs.oracle.com/javase/10/docs/api/java/util/regex/Pattern.html
- Przewodnik po wyrażeniach regularnych w Javie: https://www.tutorialspoint.com/java/java_regular_expressions.htm
- Wideo tutorial o wyrażeniach regularnych w Javie: https://www.youtube.com/watch?v=VR_nWOz2G_0