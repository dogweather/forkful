---
title:    "Java: Usuwanie znaków pasujących do wzorca"
keywords: ["Java"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/pl/java/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## Dlaczego

Jeśli pracujesz z dużymi ilościami tekstu lub stringów w swoim kodzie Java, być może zauważyłeś, że czasami musisz usuwać pewne znaki z tekstu. Na przykład, może chcesz usunąć wszystkie znaki interpunkcyjne z pewnego tekstu, aby móc lepiej analizować zawartość. W takich sytuacjach, możliwość usunięcia znaków dopasowanych do pewnego wzorca jest bardzo przydatna. W tym artykule dowiesz się, jak wykorzystać tę funkcję w języku Java.

## Jak to zrobić

Java zawiera wiele przydatnych funkcji do obsługi stringów. Jedną z nich jest metoda `replaceAll()`, która pozwala na usunięcie wszystkich wystąpień danego wzorca w tekście. Aby tego dokonać, używamy argumentu typu `regex`, czyli wyrażenia regularnego, które określa wzorzec, który chcemy usunąć.

```Java
String text = "To jest przykładowy tekst! Czy widzisz, że zawiera dużo znaków interpunkcyjnych?";
String updatedText = text.replaceAll("[!?,\\.\"]", ""); // usuwa wszystkie znaki interpunkcyjne
System.out.println(updatedText);

// Output:
// To jest przykładowy tekst Czy widzisz że zawiera dużo znaków interpunkcyjnych
```

Możemy również wykorzystać metody z klasy `Character` do sprawdzania pojedynczych znaków. Na przykład, jeśli chcemy usunąć wszystkie cyfry z tekstu, możemy użyć poniższego kodu:

```Java
String text = "10, 20, 30... kilka liczb";
StringBuilder stringBuilder = new StringBuilder();
for (char c : text.toCharArray()) {
    if (!Character.isDigit(c)) {
        stringBuilder.append(c);
    }
}
String updatedText = stringBuilder.toString();
System.out.println(updatedText);

// Output:
// , ,  kilka liczb
```

## Deep Dive

Funkcja usuwania znaków dopasowanych do wzorca może być również wykorzystywana do bardziej skomplikowanych zadań, takich jak konwersja tekstu na formatowanie CamelCase. Przykładowo, jeśli mamy tekst zapisany wyłącznie małymi literami, a chcemy go przekształcić do CamelCase, możemy zastosować metodę `replaceAll()` z odpowiednim wyrażeniem regularnym:

```Java
String text = "przykładowy_tekst_do_zmiany";
String updatedText = text.replaceAll("[_](\\w)", m -> m.group(1).toUpperCase());
System.out.println(updatedText);

// Output:
// przykładowyTekstDoZmiany
```

Wyrażenie regularne `[_](\\w)` oznacza, że chcemy znaleźć wszystkie znaki `_` i zamienić każdą następującą po nim literę na jej wersję z dużej litery. Więcej informacji na temat wyrażeń regularnych można znaleźć w linkach w sekcji "Zobacz także".

## Zobacz także
- [Java - Podstawy Stringów](https://javastart.pl/baza-wiedzy/java/typy-string/1153-wprowadzenie-do-klasy-string)
- [Poradnik po wyrażeniach regularnych w Javie](https://www.vogella.com/tutorials/JavaRegularExpressions/article.html)