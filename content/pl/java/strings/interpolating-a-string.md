---
title:                "Interpolacja łańcuchów znaków"
aliases: - /pl/java/interpolating-a-string.md
date:                  2024-01-20T17:51:00.354502-07:00
model:                 gpt-4-1106-preview
simple_title:         "Interpolacja łańcuchów znaków"

tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/java/interpolating-a-string.md"
---

{{< edit_this_page >}}

## Co i dlaczego?

Interpolacja łańcuchów to proces wstawiania wartości zmiennych do ciągu znaków. Programiści używają jej dla dynamizmu kodu i uproszczenia generowania tekstów zawierających dane.

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
