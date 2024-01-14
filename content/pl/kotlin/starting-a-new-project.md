---
title:    "Kotlin: Rozpoczynanie nowego projektu"
keywords: ["Kotlin"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/pl/kotlin/starting-a-new-project.md"
---

{{< edit_this_page >}}

## Dlaczego

Jednym z głównych powodów, dla których warto zacząć nowy projekt w języku Kotlin, jest to, że jest on szczególnie przyjazny dla programistów. Umożliwia on szybkie i efektywne pisanie aplikacji, co sprawia, że jest popularnym wyborem wśród programistów na całym świecie.

## Jak to zrobić

Dla osób zaznajomionych z językiem Java, przystosowanie się do programowania w Kotlinie powinno być wyjątkowo łatwe. Najlepszym sposobem na zapoznanie się z tym językiem jest zapisanie kilku prostych przykładów kodu. Oto przykładowy kod, który wyświetli napis "Witaj świecie!" w konsoli:

```Kotlin fun main(args: Array<String>) { println("Witaj świecie!") } ```

Uruchomienie tego kodu powinno spowodować wyświetlenie następującego wyniku:

`Witaj świecie!`

Kotlin jest również wyposażony w wiele funkcji, które są bardzo przydatne dla programistów. Na przykład można wykorzystać funkcję `when` do wykonywania różnych działań w zależności od wartości zmiennej:

```Kotlin 
fun main(args: Array<String>) { 
    var ocena = 5 
    
    when (ocena) { 
        1 -> println("Niedostateczny") 
        2 -> println("Dopuszczający") 
        3 -> println("Dostateczny") 
        4 -> println("Dobry") 
        5 -> println("Bardzo dobry") 
        6 -> println("Celujący") 
    } 
} 
```

Wynikiem tego kodu będzie:

`Bardzo dobry`

## Deep Dive

Kotlin jest językiem wieloplatformowym, co oznacza, że ​​może być używany do tworzenia aplikacji na różne platformy, takie jak Android, iOS, Windows czy Linux. Ponadto, język ten jest w pełni kompatybilny z językiem Java, dzięki czemu można w prosty sposób łączyć istniejący kod napisany w Javie z nowymi projektami w Kotlinie.

Istnieje również wiele rozszerzeń i bibliotek, które uczynią programowanie w Kotlinie jeszcze prostszym i przyjemniejszym. Na przykład, popularna biblioteka Anko oferuje wiele gotowych rozwiązań dla często powtarzających się zadań, jak na przykład tworzenie interfejsów użytkownika w aplikacjach Android.

## Zobacz także

- Oficjalna strona języka Kotlin (https://kotlinlang.org/)
- Dokumentacja języka Kotlin (https://kotlinlang.org/docs/)
- Przewodnik po języku Kotlin dla początkujących (https://kotlinlang.org/docs/tutorials/getting-started.html)