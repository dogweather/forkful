---
title:                "Rozpoczynanie nowego projektu"
html_title:           "Bash: Rozpoczynanie nowego projektu"
simple_title:         "Rozpoczynanie nowego projektu"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Getting Started"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/kotlin/starting-a-new-project.md"
---

{{< edit_this_page >}}

## Co i dlaczego?

Rozpoczęcie nowego projektu to proces tworzenia zupełnie nowego kodu od podstaw – to znakomite doświadczenie dla każdego programisty. Programiści to robią, aby wdrażać nowe pomysły, zdobywać umiejętności i rozwiązywać problemy.

## Jak to zrobić:

Zaczniemy od instalacji Kotlin w systemie:

```bash
sudo snap install --classic kotlin
```

A teraz utworzymy nasz pierwszy program Kotlin:

```Kotlin
fun main() {
    println("Witaj, Kotlin!")
}
```
Gdy uruchomimy ten kod, na ekranie zobaczymy:

```bash
Witaj, Kotlin!
```

## Pogłębiona analiza

Rozpoczęcie projektu z Kotlin ma swoje korzenie w Java, ponieważ Kotlin jest językiem JVM. Alternatywą dla Kotlin może być Java lub Scala. Jednak wielu programistów preferuje Kotlin ze względu na jego krótszą składnię i pełną interoperacyjność z Java.

Szczegóły implementacji zależą od konkretnego projektu. Jednak zasadniczo, rozpoczęcie projektu w Kotlinie obejmuje proces konfiguracji środowiska, instalacji potrzebnych narzędzi, a następnie rzeczywistego pisania kodu.

## Zobacz także

1. Dokumentacja Kotlin: https://kotlinlang.org/docs/reference/
2. Kotlin for Android Developers (Kotlin dla developerów Androida): https://developer.android.com/kotlin
3. Kotlin for Native Development (Kotlin dla natywnego developmentu): https://kotlinlang.org/docs/native-overview.html