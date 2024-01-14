---
title:    "Kotlin: Sprawdzanie istnienia katalogu"
keywords: ["Kotlin"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/pl/kotlin/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## Dlaczego

Sprawdzanie, czy istnieje katalog w programowaniu Kotlin może być nieuniknionym krokiem w niektórych projektach. Warto mieć tę umiejętność w swoim arsenału, aby móc skutecznie zarządzać plikami i katalogami w aplikacji.

## Jak to zrobić

```Kotlin
val directory = File("nazwa_katalogu")
if(!directory.exists()){
    println("Katalog nie istnieje")
}else{
    println("Katalog istnieje")
}
```

W powyższym przykładzie tworzymy obiekt typu File, który reprezentuje nasz katalog. Następnie sprawdzamy, czy istnieje przy użyciu metody `exists()`. Jeśli wynik jest fałszywy, oznacza to, że katalog nie istnieje, a jeśli jest prawdziwy, to katalog istnieje.

Możemy także użyć wyrażenia zwracającego wartość logiczną w instrukcji if:

```Kotlin
if(File("nazwa_katalogu").exists()){
    println("Katalog istnieje")
}else{
    println("Katalog nie istnieje")
}
```

## Deep Dive

Sprawdzanie, czy istnieje dany katalog polega na tym, że program sprawdza system plików w poszukiwaniu nazwy katalogu. Jeśli znajdzie katalog o takiej samej nazwie, zwraca wartość prawdziwą, a jeśli nie, zwraca wartość fałszywą.

Warto wiedzieć, że metoda `exists()` zwraca również wartość prawdziwą, jeśli podamy nazwę pliku, a nie katalogu. Dlatego ważne jest, aby wcześniej dokładnie sprawdzić typ obiektu i czy faktycznie sprawdzamy istnienie katalogu.

## Zobacz również

- [Oficjalna dokumentacja Kotlin na temat klas File i Path](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin.io/java.io.-file/index.html)
- [Porównanie szybkości działania metod exists() i isDirectory() w Kotlin](https://stackabuse.com/kotlincheck-if-directory-exists/)
- [Przykładowy projekt w Kotlin, w którym wykorzystano sprawdzanie istnienia katalogu](https://github.com/krzysztofkrystianowicz/spring-kotlin-files-upload)