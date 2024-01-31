---
title:                "Korzystanie z interaktywnego shella (REPL)"
date:                  2024-01-26T04:15:21.503483-07:00
model:                 gpt-4-0125-preview
simple_title:         "Korzystanie z interaktywnego shella (REPL)"

category:             "Java"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/java/using-an-interactive-shell-repl.md"
---

{{< edit_this_page >}}

## Co i dlaczego?
REPL (Read-Eval-Print Loop), czyli pętla czytaj-wykonaj-drukuj, to interaktywna powłoka, która przetwarza pojedyncze wejścia użytkownika, wykonuje kod i zwraca wynik. Programiści używają jej do szybkich eksperymentów, debugowania lub nauki, ponieważ umożliwia natychmiastową informację zwrotną i iterację.

## Jak to zrobić:
Uruchomienie REPL w Javie jest proste dzięki narzędziu `jshell` wprowadzonemu w Java 9. Oto jak się za to zabrać i rozpocząć podstawową sesję:

```Java
jshell> int sum(int a, int b) {
   ...> return a + b;
   ...> }
|  utworzono metodę sum(int,int)

jshell> sum(5, 7)
$1 ==> 12
```

Wyjście w dowolnym momencie za pomocą `/exit`.

```Java
jshell> /exit
|  Do widzenia
```

## Zagłębienie się
Przed `jshell`, programiści Javy nie mieli oficjalnego REPL, w przeciwieństwie do programistów Pythona czy Ruby. Używali IDE lub pisali pełne programy nawet do trywialnych zadań. `jshell` był rewolucją od czasu Java 9, zamykając tę lukę.

Do alternatyw należą kompilatory online lub wtyczki do IDE, ale nie dorównują one natychmiastowości `jshell`. Jeśli chodzi o szczegóły, `jshell` używa Java Compiler API do wykonywania fragmentów kodu, co jest całkiem sprytne. To coś więcej niż plac zabaw – można importować biblioteki, definiować klasy i wiele więcej. To sprawia, że jest to solidne narzędzie do prototypowania.

## Zobacz także
- [Przewodnik użytkownika JShell](https://docs.oracle.com/javase/9/jshell/introduction-jshell.htm)
- [Referencje narzędzi Java Platform, Standard Edition](https://docs.oracle.com/javase/9/tools/tools-and-command-reference.htm#JSWOR719)
- [Java Compiler API](https://docs.oracle.com/javase/9/docs/api/javax/tools/JavaCompiler.html)
