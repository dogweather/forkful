---
title:    "Java: Usuwanie znaków pasujących do wzorca"
keywords: ["Java"]
---

{{< edit_this_page >}}

# Dlaczego warto usunąć znaki pasujące do wzorca?

Często w programowaniu spotykamy się z sytuacją, w której musimy usunąć niektóre znaki z tekstu, które pasują do określonego wzorca. Może to być potrzebne, na przykład, podczas przetwarzania danych lub usuwania niechcianych elementów z tekstu. W tym artykule opowiemy o sposobach usuwania znaków pasujących do wzorca w języku Java.

## Jak to zrobić?

W Javie istnieje wiele metod, które umożliwiają usuwanie znaków pasujących do wzorca. Jedną z najprostszych jest użycie metody `replaceAll()` w klasie `String`. Poniżej znajduje się przykładowy kod, który usuwa wszystkie spacje z podanego tekstu:

```Java
String tekst = "To jest przykładowy tekst.";
String nowyTekst = tekst.replaceAll(" ", "");
System.out.println(nowyTekst);
```

Wynik wyświetlony na konsoli będzie wyglądał następująco:

```Java
Tojestprzykładowytekst.
```

Możemy również użyć wyrażenia regularnego w metodzie `replaceAll()` aby usunąć wszystkie znaki nie będące literami. Poniżej przedstawiamy kod oraz wynik działania.

```Java
String tekst = "1234 abcd !@$%";
String nowyTekst = tekst.replaceAll("[^a-zA-Z]", "");
System.out.println(nowyTekst);
```

Wynik wyświetlony na konsoli będzie wyglądał następująco:

```Java
abcd
```

## Zanurzenie w temat

Chociaż `replaceAll()` jest prosta w użyciu, może być również kosztowna wydajnościowo dla większych tekstów. W takim przypadku lepszym rozwiązaniem jest użycie klasy `StringBuilder` i metody `deleteCharAt()` do usuwania znaków pasujących do wzorca. Poniżej przedstawiamy przykładowy kod oraz wynik działania.

```Java
StringBuilder builder = new StringBuilder("To jest przykładowy tekst.");
for(int i = 0; i < builder.length(); i++) {
    if(builder.charAt(i) == ' ') {
        builder.deleteCharAt(i);
    }
}
String nowyTekst = builder.toString();
System.out.println(nowyTekst);
```

Wynik wyświetlony na konsoli będzie wyglądał następująco:

```Java
Tojestprzykładowytekst.
```

Poza tym, istnieją różne metody i klasy w bibliotekach Apache Commons i Guava, które ułatwiają usuwanie znaków pasujących do wzorca. Warto zapoznać się również z tymi narzędziami i wybrać najbardziej odpowiednie dla naszego przypadku.

# Zobacz również

- [Metoda replaceAll() w Javie](https://docs.oracle.com/javase/7/docs/api/java/lang/String.html#replaceAll%28java.lang.String%2C%20java.lang.String%29)
- [Klasa StringBuilder w Javie](https://docs.oracle.com/javase/7/docs/api/java/lang/StringBuilder.html)
- [Apache Commons Lang - klasa StringUtils](https://commons.apache.org/proper/commons-lang/apidocs/org/apache/commons/lang3/StringUtils.html)
- [Guava lib - klasa CharMatcher](https://github.com/google/guava/wiki/StringsExplained#1-CharMatcher)