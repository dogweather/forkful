---
title:                "Używanie wyrażeń regularnych"
html_title:           "Java: Używanie wyrażeń regularnych"
simple_title:         "Używanie wyrażeń regularnych"
programming_language: "Java"
category:             "Java"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/java/using-regular-expressions.md"
---

{{< edit_this_page >}}

## Dlaczego

Regular expressions, zwane również wyrażeniami regularnymi, są bardzo użytecznym narzędziem dla programistów Java. Pozwalają one na łatwe i precyzyjne wyszukiwanie i manipulowanie tekstem, co znacznie ułatwia pracę z łańcuchami znaków w kodzie.

## Jak to zrobić

Regular expressions są dostępne w Javie dzięki wyrażeniom typu `Pattern` oraz `Matcher`. Najprostszym sposobem na rozpoczęcie używania wyrażeń regularnych jest użycie statycznej metody `compile()` klasy `Pattern`, która przyjmuje jako argument wyrażenie regularne oraz opcje wyszukiwania (np. wielkość liter). Poniższy przykład pokazuje, jak znaleźć wszystkie wystąpienia słowa "Java" w tekście i zwrócić ich indeksy:

```Java
Pattern pattern = Pattern.compile("Java");
Matcher matcher = pattern.matcher("Java jest niesamowita!");
while (matcher.find()) {
    System.out.println(matcher.start());
}
```
**Output:** 0 17

Można także używać wyrażeń regularnych do wyciągania konkretnych fragmentów tekstu za pomocą tzw. grupy. W poniższym przykładzie, pobieramy wszystkie liczby z ciągu znaków i zwracamy je jako tablicę:

```Java
Pattern pattern = Pattern.compile("\\d+"); // znak "\" przed znakiem "+" jest wymagany, aby oznaczać własny znak "+"
Matcher matcher = pattern.matcher("Mam 24 lata i ważę 60 kg");
while (matcher.find()) {
    System.out.println(matcher.group());
}
```
**Output:** 24 60

Warto także poznać kilka przydatnych wyrażeń regularnych. Na przykład, `.` oznacza dowolny pojedynczy znak, `*` oznacza dowolną ilość powtórzeń poprzedniego znaku, a `?` oznacza, że poprzedni znak jest opcjonalny. Więcej przykładów znajdziesz w sekcji Deep Dive.

## Deep Dive

Regular expressions są bardzo wszechstronnym narzędziem, dlatego warto poznać kilka ich zaawansowanych funkcjonalności. Na przykład, można użyć `|` aby oznaczyć alternatywne wyrażenie, np. `Java|Python` będzie pasowało do obu słów. Można także używać znaku `^` na początku wyrażenia, aby wyszukiwać tylko na początku tekstu. Aby precyzyjniej wyszukiwać, można używać zbiorów znaków, np. `[A-Za-z]` będzie pasowało do dużych i małych liter.

Warto także pamiętać o specjalnym znaku `\` w wyrażeniach regularnych. Jeśli chcesz użyć znaku specjalnego (np. `*`) jako zwykłego znaku, musisz go poprzedzić znakiem `\`, w przeciwnym razie zostanie on zinterpretowany jako znak specjalny.

## Zobacz także

- [Dokumentacja Javy na temat wyrażeń regularnych](https://docs.oracle.com/javase/8/docs/api/java/util/regex/Pattern.html)
- [Kurs wyrażeń regularnych w Javie](https://www.javatpoint.com/java-regex)