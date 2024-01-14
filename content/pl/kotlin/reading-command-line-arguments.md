---
title:                "Kotlin: Odczytywanie argumentów wiersza poleceń"
programming_language: "Kotlin"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/kotlin/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## Dlaczego
Jeśli planujesz rozwijać swoją wiedzę i umiejętności w zakresie programowania w języku Kotlin, z pewnością należy zapoznać się z możliwościami czytania argumentów wiersza poleceń. Dzięki temu zyskasz umiejętność dynamicznego zarządzania swoim kodem i zwiększysz swoją produktywność. Czytanie argumentów wiersza poleceń może być wykorzystane w różnych projektach, od prostych skryptów po duże i złożone aplikacje.

## Jak to zrobić
Aby czytać argumenty wiersza poleceń w języku Kotlin, wykorzystaj metodę `args` z obiektu `main` w swoim pliku `Main.kt`. Poniższy przykład kodu pokazuje, jak uzyskać dostęp do pojedynczych argumentów i wypisać je na ekranie:

```Kotlin
fun main(args: Array<String>) {
    // pobranie pierwszego argumentu
    val firstArgument = args[0]
    println("Pierwszy argument to: $firstArgument")

    // pobranie drugiego argumentu
    val secondArgument = args[1]
    println("Drugi argument to: $secondArgument")
}
```

Pamiętaj, że indeksy w tablicy `args` zaczynają się od 0, więc pierwszy argument będzie dostępny pod indeksem `0`, drugi pod `1`, itd. Możesz również użyć pętli `for` do iteracji przez wszystkie argumenty, na przykład:

```Kotlin
for (argument in args) {
    println(argument)
}
```

Powyższy kod wypisze na ekranie wszystkie argumenty wpisane przy uruchamianiu programu. Dzięki temu możesz dynamicznie przetwarzać dane wejściowe i dostosowywać działanie aplikacji do różnych scenariuszy.

## Głębsze spojrzenie
Istnieją również inne sposoby na czytanie argumentów wiersza poleceń w języku Kotlin. Niektóre z nich wymagają bibliotek zewnętrznych, które oferują zaawansowane funkcje, takie jak parsowanie argumentów pod kątem określonych flag lub użycie specjalnej składni. Jednak wykorzystanie metody `args` jest wystarczające dla podstawowych potrzeb i wydajniejsze w przypadku prostych zastosowań.

## Zobacz również
- Dokumentacja języka Kotlin: [Czytanie argumentów wiersza poleceń](https://kotlinlang.org/docs/reference/basic-types.html#command-line-arguments)
- Poradnik na blogu AndroidPortal: [Obsługa argumentów wiersza poleceń w języku Kotlin](https://androidportal.pl/kotlin/obsLuga-argumentow-wiersza-polecen-w-jezyku-kotlin/)