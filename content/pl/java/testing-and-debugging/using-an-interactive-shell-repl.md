---
date: 2024-01-26 04:15:21.503483-07:00
description: "Jak to zrobi\u0107: Uruchomienie REPL w Javie jest proste dzi\u0119\
  ki narz\u0119dziu `jshell` wprowadzonemu w Java 9. Oto jak si\u0119 za to zabra\u0107\
  \ i rozpocz\u0105\u0107 podstawow\u0105\u2026"
lastmod: '2024-03-13T22:44:35.278928-06:00'
model: gpt-4-0125-preview
summary: "Uruchomienie REPL w Javie jest proste dzi\u0119ki narz\u0119dziu `jshell`\
  \ wprowadzonemu w Java 9."
title: Korzystanie z interaktywnego shella (REPL)
weight: 34
---

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
