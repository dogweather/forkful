---
title:    "Kotlin: Sprawdzanie istnienia katalogu"
keywords: ["Kotlin"]
---

{{< edit_this_page >}}

# Dlaczego warto sprawdzić, czy istnieje katalog

Sprawdzanie czy katalog istnieje jest istotnym krokiem w procesie tworzenia aplikacji w Kotlinie. Dzięki takiemu działaniu możemy zapewnić, że nasza aplikacja będzie działała poprawnie i uniknąć błędów związanych z nieistniejącymi katalogami.

# Jak to zrobić

```Kotlin
fun checkDirectoryExists(directory: File): Boolean{
   if (directory.exists()) {
        println("${directory.name} istnieje.")
        return true
    } else {
        println("${directory.name} nie istnieje.")
        return false
    }
}
```

W powyższym kodzie tworzymy funkcję, która przyjmuje jako argument obiekt typu File - nasz katalog, który chcemy sprawdzić. Wykorzystujemy metodę `exists()`, która zwraca wartość `true`, jeśli plik lub katalog istnieje, a `false`, jeśli nie istnieje. Następnie wyświetlamy odpowiedni komunikat i zwracamy odpowiednią wartość. Aby wywołać funkcję, możemy użyć następującego kodu:

```Kotlin
val directory = File("sciezka/do/katalogu")
checkDirectoryExists(directory)
```

W konsoli otrzymamy informację czy katalog istnieje, a jeśli nie - jaki katalog nie został znaleziony.

# Głębszy wgląd

Sprawdzanie czy katalog istnieje jest często wykorzystywane w przypadku, gdy nasza aplikacja musi pobrać lub zapisać plik w danym katalogu. W takiej sytuacji możemy wykorzystać wcześniej stworzoną funkcję, aby sprawdzić czy katalog istnieje przed próbą odwołania się do niego. Dzięki temu unikniemy błędów w naszym programie.

Dodatkowo, warto zauważyć, że metoda `exists()` może zwracać również wartość `false` w przypadku, gdy nie mamy uprawnień do danego katalogu. W takiej sytuacji powinniśmy wyświetlić odpowiedni komunikat użytkownikowi.

# Zobacz także

* [Dokumentacja Kodu Kotlin](https://kotlinlang.org/docs/reference/)
* [Poradnik dla początkujących w języku Kotlin](https://medium.com/@krystiankoper/od-zera-do-bohatera-seria-wprowadzenie-do-j%C4%99zyka-kotlin-4bad1dde6f82)
* [Kotlin dla Java developerów](https://blog.jetbrains.com/kotlin/2019/01/kotlin-for-java-developers/)

Dziękujemy za przeczytanie naszego bloga na temat sprawdzania czy katalog istnieje w języku Kotlin. Mamy nadzieję, że ten wpis był dla ciebie pomocny i przyczynił się do lepszego zrozumienia tego zagadnienia. Oczywiście, istnieje wiele innych sytuacji, w których możemy wykorzystać tę wiedzę, dlatego zachęcamy do dalszej eksploracji języka Kotlin. Powodzenia!