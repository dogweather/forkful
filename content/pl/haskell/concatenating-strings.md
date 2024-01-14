---
title:    "Haskell: Konkatenacja ciągów znakowych"
keywords: ["Haskell"]
---

{{< edit_this_page >}}

##Dlaczego

Czsto podczas pisania kodu w Haskellu, programici potrzebuj concatenacji napisów (czyli łączenia kilku osobnych napisów w jeden). Ten prosty sposb jest używany w wielu aplikacjach, na przykład do tworzenia komunikatów dla użytkownika lub wypisywania tekstu na ekranie.

##Jak to zrobić

Aby połączyć dwa napisy w Haskellu, można użyć funkcji `++`. Na przykład, jeśli mamy dwa napisy: "Witaj" i "świecie", można je połączyć za pomocą następującego kodu:

```Haskell
concatenatedString = "Witaj" ++ "świecie"
```

Po uruchomieniu tego kodu, zmienna `concatenatedString` będzie zawierać napis "Witajświecie". Można również połączyć więcej niż dwa napisy, dodając kolejne `++` między nimi.

```Haskell
longString = "To jest" ++ "bardzo" ++ "długi" ++ "napis"
```

Rezultatem tego kodu będzie napis "To jestbardzodługinapis".

Warto również zauważyć, że funkcja `++` działa na dowolnych typach danych, nie tylko na napisach. Można ją wykorzystać do łączenia list, np. `[1, 2] ++ [3, 4]` da nam listę `[1, 2, 3, 4]`.

##Głębsze wyjaśnienie

W Haskellu napisy są reprezentowane jako listy znaków, więc używanie funkcji `++` jest w istocie po prostu łączeniem dwóch list. Może to być nieco nieintuicyjne dla początkujących programistów, którzy oczekują, że funkcja łącząca napisy będzie działać podobnie jak w innych językach programowania.

Ponadto, w Haskellu istnieje również funkcja `concat`, która może być użyta do łączenia list. Jednak, w przeciwieństwie do `++`, `concat` działa tylko na listach list (czyli lista list znaków), a nie na pojedynczych elementach. W większości przypadków jednak, funkcja `++` jest wystarczająca do łączenia napisów.

##Zobacz również

[Oficjalna dokumentacja dla funkcji `++`](https://hackage.haskell.org/package/base-4.14.1.0/docs/Prelude.html#v:-43--43-) [Oficjalna dokumentacja dla funkcji `concat`](https://hackage.haskell.org/package/base-4.14.1.0/docs/Prelude.html#v:concat) [Inne przydatne funkcje dla napisów w Haskellu](https://www.tutorialspoint.com/haskell/haskell_strings.htm)