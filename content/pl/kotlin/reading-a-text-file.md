---
title:    "Kotlin: Odczytywanie pliku tekstowego"
keywords: ["Kotlin"]
---

{{< edit_this_page >}}

# Dlaczego czytać plik tekstowy?

Często w ramach programowania spotykamy się z koniecznością przetwarzania plików tekstowych. Mogą one zawierać różnego rodzaju informacje, takie jak dane kontaktowe, dane finansowe lub nawet kody źródłowe. Dlatego też warto poznać podstawy czytania plików tekstowych w języku Kotlin.

# Jak to zrobić?

Aby przeczytać plik tekstowy w języku Kotlin, wystarczy kilka prostych kroków:

1. Zaimportuj klasę `File` z pakietu `java.io` przy pomocy słowa kluczowego `import`. Dzięki temu będziesz miał dostęp do metod do obsługi plików.

2. Utwórz obiekt klasy `File` podając ścieżkę do pliku jako argument. Możesz wykorzystać zarówno bezwzględną jak i względną ścieżkę do pliku.

3. Wykorzystaj metodę `readLines()` na utworzonym obiekcie `File`, aby odczytać zawartość pliku i przypisać ją do zmiennej typu `List<String>`.

4. Możesz teraz wykorzystać tę zmienną do manipulowania danymi z pliku według swoich potrzeb.

Przykładowy kod:

```Kotlin
import java.io.File

val file = File("plik.txt")
val lines: List<String> = file.readLines()
```

# Dogłębna analiza

Powyższe kroki pozwalają na podstawowe czytanie pliku tekstowego w języku Kotlin. Możesz jednak zauważyć, że wykorzystujemy tu klasę `File` z pakietu `java.io`, a nie wbudowany w język Kotlin mechanizm do obsługi plików. Dlaczego tak się dzieje?

Otóż język Kotlin jest zintegrowany z platformą Javy, co oznacza, że korzysta z jej bibliotek i klas. Klasa `File` jest częścią standardowej biblioteki Javy i wykorzystując ją, możemy uzyskać dostęp do wielu przydatnych metod do obsługi plików.

W języku Kotlin jest również dostępna klasa `File` w pakiecie `kotlin.io`, ale jest ona uznawana za eksperymentalną i może ulec zmianie w przyszłych wersjach języka.

# Zobacz też

- Dokumentacja klasy `File` w języku Java: https://docs.oracle.com/javase/8/docs/api/java/io/File.html
- Dokumentacja kotlinowej klasy `File`: https://kotlinlang.org/api/latest/jvm/stdlib/kotlin.io/java.io.-file/
- Przykładowa aplikacja wykorzystująca czytanie pliku tekstowego w Kotlinie: https://github.com/JaneKeller/reading-files-with-kotlin