---
title:    "Java: Używając wyrażeń regularnych"
keywords: ["Java"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/pl/java/using-regular-expressions.md"
---

{{< edit_this_page >}}

## Dlaczego

Jeśli jesteś programistą Java, na pewno już słyszałeś o wyrażeniach regularnych. Ale dlaczego warto się nimi zainteresować? Wyrażenia regularne są bardzo potężnym narzędziem, które pozwala na wykonywanie złożonych operacji na ciągach znaków. Dzięki nim możesz szybko i skutecznie przetwarzać dane, weryfikować poprawność wprowadzanych informacji, czy też wyodrębniać potrzebne informacje z dużych zbiorów tekstu. W tym artykule pokażemy Ci, jak wykorzystać wyrażenia regularne w języku Java.

## Jak to zrobić

Aby używać wyrażeń regularnych w języku Java, musisz najpierw zaimportować klasę `java.util.regex`. Następnie możesz tworzyć wzorce i wykorzystywać je do przetwarzania tekstu. Na przykład, jeśli chcesz sprawdzić, czy dany ciąg znaków zawiera tylko cyfry, możesz użyć następującego kodu:

```Java
String text = "12345";
if (text.matches("\\d+")) {
    System.out.println("Ciąg zawiera tylko cyfry!");
}
```

Zobacz, jak używamy metody `matches` i przekazujemy do niej część wzorca `\d+`, które oznacza, że musi wystąpić co najmniej jedna cyfra. Możesz również użyć innych znaków specjalnych, takich jak `*`, `+` czy `?`, aby precyzyjniej określić wzorzec.

Możesz także wykorzystać wyrażenia regularne do wyodrębnienia informacji z tekstu. Na przykład, jeśli chcemy wyodrębnić wszystkie adresy email z danego dokumentu, możemy użyć poniższego kodu:

```Java
String text = "To jest mój adres email: example@email.com. 
Inny email to: another@gmail.com";
Pattern pattern = Pattern.compile("\\w+@\\w+\\.\\w+");
Matcher matcher = pattern.matcher(text);

while (matcher.find()) {
    System.out.println(matcher.group());
}
```

Zwróć uwagę na użycie metody `find`, która odnajduje wszystkie dopasowania do wzorca i zwraca je w grupach, z których pierwsza to całe dopasowanie.

## Głębsza analiza

Wyrażenia regularne to nie tylko proste sprawdzanie czy ciąg zawiera określone znaki. Dzięki nim możesz również stosować odpowiednie wyrażenia zastępcze, grupy i wyrażenia warunkowe, co daje możliwość tworzenia bardziej zaawansowanych wzorców.

Na przykład, jeśli chcesz sprawdzić poprawność numeru PESEL, możesz użyć następującego wzorca:

```Java
\\d{11}
```

Jednakże, możesz również wykorzystać wyrażenia warunkowe, aby sprawdzić, czy ostatni cyfra jest zgodna z określonym algorytmem. W tym przypadku, możemy doprecyzować wzorzec:

```Java
(\\d{2})(\\d{2})(\\d{2})(\\d{2})(\\d{2})(\\d{2})(\\d{1})(?(?=\\1)[13579][02468]|[02468][13579])
```

Ten wzorzec sprawdzi, czy pierwsze sześć cyfr jest poprawnego formatu, a ostatnia cyfra odpowiada za sprawdzenie poprawności numeru.

## Zobacz również

- Dokumentacja Java dotycząca wyrażeń regularnych: https://docs.oracle.com/javase/8/docs/api/java/util/regex/Pattern.html
- Przewodnik po wyrażeniach regularnych w Java: https://www.vogella.com/tutorials/JavaRegularExpressions/article.html
- Przykładowe zadania z wykorzystaniem wyrażeń regularnych w Java: https://