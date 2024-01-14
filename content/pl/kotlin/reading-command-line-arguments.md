---
title:                "Kotlin: Odczytywanie argumentów wiersza poleceń"
simple_title:         "Odczytywanie argumentów wiersza poleceń"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/kotlin/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## Dlaczego

Czy kiedykolwiek słyszałeś o przekazaniu argumentów przez wiersz poleceń do swojego programu? Jeśli tak, być może zastanawiasz się, dlaczego jest to potrzebne lub przydatne. W tym artykule dowiesz się, dlaczego warto poznać tę funkcję w języku Kotlin.

## Jak to zrobić

Poznanie przekazywania argumentów przez wiersz poleceń w języku Kotlin jest bardzo proste. Wystarczy użyć wbudowanej funkcji "args" w głównym pliku programu. Poniżej przedstawiony jest przykładowy kod, który drukuje wszystkie przekazane argumenty w terminalu:

```Kotlin
fun main(args: Array<String>) {
    for (arg in args) {
        println(arg)
    }
}
```

Jeśli uruchomisz ten program z argumentami "Hello", "world!", to w konsoli uzyskasz następujący wynik:

```
Hello
world!
```

## Deep Dive

W języku Kotlin możesz przekazywać argumenty wiersza poleceń nie tylko podczas uruchamiania programu, ale także w trakcie jego działania. Na przykład, jeśli użyjesz funkcji "readLine" w celu wprowadzenia argumentów przez użytkownika, zostaną one przekazane jako argumenty wiersza poleceń. Takie podejście może być bardzo przydatne, zwłaszcza w przypadku aplikacji konsolowych.

Ponadto, możesz również określić typy danych argumentów wiersza poleceń, co ułatwia ich przetwarzanie. Na przykład, jeśli wiesz, że oczekiwane argumenty będą liczbami, możesz użyć funkcji "toInt" lub "toDouble" do ich przekonwertowania przed dalszym przetwarzaniem.

## Zobacz również

Jeśli chcesz dowiedzieć się więcej o przekazywaniu argumentów przez wiersz poleceń w języku Kotlin, polecam zapoznać się z dokumentacją języka oraz innych poradników dostępnych online:

- Dokumentacja języka Kotlin: https://kotlinlang.org/docs/reference/command-line.html
- Poradnik wideo na YouTube: https://www.youtube.com/watch?v=bw-cXwFWfWY
- Artykuł na blogu Medium: https://medium.com/@thomasviana/kotlin-command-line-arguments-e7bd02f64ada