---
title:                "Łączenie ciągów znaków"
html_title:           "Arduino: Łączenie ciągów znaków"
simple_title:         "Łączenie ciągów znaków"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/arduino/concatenating-strings.md"
---

{{< edit_this_page >}}

## Dlaczego

Czy kiedykolwiek zastanawiałeś się, dlaczego warto łączyć łańcuchy w programowaniu Arduino? Może wydawać się to zbędne, ale istnieją wiele sytuacji, w których konieczne będzie połączenie kilku łańcuchów w jedną linię tekstu. W tym artykule dowiesz się dlaczego i jak to zrobić.

## Jak to zrobić

Arduino oferuje nam różne sposoby na łączenie łańcuchów, ale dzisiaj skupimy się na trzech najważniejszych: operator "+" (plus), funkcja "concat()" oraz funkcja "sprintf()". 

```Arduino
String tekst = "Witaj";
String imie = "Jan";
String powitanie = tekst + " " + imie;
// wynik: "Witaj Jan"
```

```Arduino
String pierwszy = "Arduino";
String drugi = "jest";
String trzeci = "fajne";
String zdanie = pierwszy.concat(" ", drugi, " ", trzeci);
// wynik: "Arduino jest fajne"
```

```Arduino
char zdanie[20];
char pierwszy[] = "Hello";
char drugi[] = "world";
sprintf(zdanie, "%s %s", pierwszy, drugi);
// wynik: "Hello world"
```

## Deep Dive

Połączenie łańcuchów może być przydatne w różnych sytuacjach, np.:

- tworzenie dynamicznych komunikatów w zależności od wartości sensora
- tworzenie dłuższych linii tekstu w prosty sposób
- łączenie nazw zmiennych z wartościami w celu wyświetlenia informacji na ekranie

Warto również zwrócić uwagę na to, że wszystkie wymienione powyżej sposoby łączenia łańcuchów są dostępne również w wielu innych językach programowania, więc poznając je na Arduino, zyskujesz umiejętności, które możesz wykorzystać również w innych projektach.

## Zobacz również

- Dokumentacja Arduino na temat łączenia łańcuchów: https://www.arduino.cc/reference/tr/language/functions/string-functions/concat/
- Przykładowe projekty wykorzystujące łączenie łańcuchów w Arduino: https://www.hackster.io/search?i=projects&q=concatenating%20strings%20arduino
- Wprowadzenie do podstaw programowania w Arduino: https://euske.github.io/arduino/samples/hello.html