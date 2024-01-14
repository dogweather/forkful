---
title:                "Kotlin: Sprawdzanie, czy istnieje katalog."
programming_language: "Kotlin"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/kotlin/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## Dlaczego

Jeśli jesteś programistą, w pewnym momencie możesz potrzebować sprawdzić, czy dany katalog istnieje w swoim kodzie. Niezależnie od tego, czy jest to wymóg Twojej aplikacji czy też potrzeba wykonania pewnych operacji związanych z danym katalogiem, poradnik ten pomoże Ci w wykonywaniu tej prostej czynności.

## Jak to zrobić

Sprawdzenie istnienia katalogu w języku Kotlin jest bardzo proste. Wystarczy użyć funkcji `exists()` z klasy `File` i podać ścieżkę do katalogu jako parametr. Oto przykładowy kod:

```Kotlin
val dir = File("ścieżka/do/katalogu")
if (dir.exists()){
    println("Katalog istnieje!")
} else {
    println("Katalog nie istnieje.")
}
```

W powyższym przykładzie utworzono obiekt klasy `File` reprezentujący dany katalog, a następnie wywołano na nim funkcję `exists()`, która zwraca `true` lub `false` w zależności od tego, czy katalog istnieje. W zależności od wyniku, wypisywany jest odpowiedni komunikat.

## Deep Dive

Podczas wykonywania funkcji `exists()`, system operacyjny wykonuje operację sprawdzania istnienia pliku lub katalogu. Jeśli wynik jest pozytywny, zwracany jest `true`, w przeciwnym przypadku zwracane jest `false`. Jest to bardzo prosta, ale ważna operacja, ponieważ pozwala nam na podejmowanie odpowiednich działań w zależności od istnienia danego katalogu.

## Zobacz również

- Dokumentacja Kotlin: [File.exists](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin.io/java.io.-file/exists.html)
- Poradnik: [Jak sprawdzić, czy plik istnieje w języku Kotlin](https://www.baeldung.com/java-kotlin-file-exists)

Dziękujemy za przeczytanie tego krótkiego poradnika. Mamy nadzieję, że teraz wiesz, jak łatwo i szybko sprawdzić istnienie danego katalogu w języku Kotlin. Powodzenia w dalszym programowaniu!