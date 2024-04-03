---
date: 2024-01-20 17:51:00.354502-07:00
description: "Interpolacja \u0142a\u0144cuch\xF3w to proces wstawiania warto\u015B\
  ci zmiennych do ci\u0105gu znak\xF3w. Programi\u015Bci u\u017Cywaj\u0105 jej dla\
  \ dynamizmu kodu i uproszczenia generowania\u2026"
lastmod: '2024-03-13T22:44:35.263240-06:00'
model: gpt-4-1106-preview
summary: "Interpolacja \u0142a\u0144cuch\xF3w to proces wstawiania warto\u015Bci zmiennych\
  \ do ci\u0105gu znak\xF3w."
title: "Interpolacja \u0142a\u0144cuch\xF3w znak\xF3w"
weight: 8
---

## Jak to zrobić:
W Java (jak w wersji 15 i późniejszych) możemy użyć text blocks oraz metody `formatted()`, aby łatwo interpolować stringi. Oto przykład:

```java
public class StringInterpolationExample {
    public static void main(String[] args) {
        String name = "Łukasz";
        int age = 29;
        String greeting = "Cześć, %s. Masz %d lat.".formatted(name, age);
        System.out.println(greeting);
    }
}
```

Wyjście:
```
Cześć, Łukasz. Masz 29 lat.
```

## Pogłębienie:
Interpolacja stringów pojawiła się w wielu językach przed Java, ale Java długo polegała na konkatenacji z użyciem operatora `+` lub `StringBuilder`. Opcje takie jak `String.format()` czy `MessageFormat` były dostępne, ale interpolacja stringów stała się znacznie wygodniejsza i bardziej czytelna dzięki wprowadzeniu text blocks w Java 15.

Alternatywy obejmują użycie `String.format()`, łańcuchów z konkatenacją i `StringBuilder` dla starszych wersji Javy:

```java
String name = "Łukasz";
int age = 29;
String greeting = String.format("Cześć, %s. Masz %d lat.", name, age);
```

Interpolacja jest po prostu syntaktycznym cukrem, który upraszcza sposób, w jaki wstawiamy zmienne do łańcuchów znaków, zazwyczaj poprzez wewnętrzną zamianę na `String.format()` lub podobne implementacje.

## Zobacz również:
- [Dokumentacja Oracle dla text blocks](https://docs.oracle.com/en/java/javase/15/text-blocks/index.html)
- [Dokumentacja Oracle dla metody `formatted()`](https://docs.oracle.com/en/java/javase/15/docs/api/java.base/java/lang/String.html#formatted(java.lang.Object...))
- [Przewodnik Oracle'a do `String.format()`](https://docs.oracle.com/javase/8/docs/api/java/lang/String.html#format-java.lang.String-java.lang.Object...-)
