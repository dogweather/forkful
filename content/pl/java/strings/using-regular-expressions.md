---
title:                "Korzystanie z wyrażeń regularnych"
aliases:
- /pl/java/using-regular-expressions.md
date:                  2024-02-03T19:17:18.339899-07:00
model:                 gpt-4-0125-preview
simple_title:         "Korzystanie z wyrażeń regularnych"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/java/using-regular-expressions.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Co i dlaczego?

Wyrażenia regularne (regex) w Javie pozwalają definiować specyficzne wzorce do wyszukiwania, manipulowania lub walidacji ciągów znaków w twoim kodzie. Programiści używają ich do zadań takich jak parsowanie plików logów, walidacja danych użytkownika lub wyszukiwanie konkretnych wzorców w tekście, umożliwiając zaawansowane przetwarzanie ciągów znaków z minimalnym wysiłkiem.

## Jak to zrobić:

Wbudowane wsparcie dla regex w Javie zapewniają przede wszystkim klasy `Pattern` i `Matcher` z pakietu `java.util.regex`. Oto prosty przykład, jak znaleźć i wydrukować wszystkie wystąpienia słowa w ciągu znaków, niezależnie od wielkości liter:

```java
import java.util.regex.Matcher;
import java.util.regex.Pattern;

public class RegexExample {
    public static void main(String[] args) {
        String text = "Regex is great for parsing. Parsing with regex is powerful.";
        String wordToFind = "parsing";
        
        Pattern pattern = Pattern.compile(wordToFind, Pattern.CASE_INSENSITIVE);
        Matcher matcher = pattern.matcher(text);
        
        while (matcher.find()) {
            System.out.println("Znaleziono '" + matcher.group() + "' na pozycji " + matcher.start());
        }
    }
}
```

Wyjście:
```
Znaleziono 'parsing' na pozycji 16
Znaleziono 'Parsing' na pozycji 31
```

Do zadań takich jak dzielenie ciągów znaków możesz użyć metody `split()` klasy `String` z regex:

```java
public class SplitExample {
    public static void main(String[] args) {
        String text = "Java,Python,Ruby,JavaScript";
        String[] languages = text.split(",");
        
        for (String language : languages) {
            System.out.println(language);
        }
    }
}
```

Wyjście:
```
Java
Python
Ruby
JavaScript
```

Pracując z regex w Javie, mogą być przypadki, gdy biblioteka zewnętrzna może uprościć złożone zadania. Jedną z popularnych bibliotek stron trzecich do pracy z regex w Javie jest `Apache Commons Lang`. Oferuje narzędzia takie jak `StringUtils`, które sprawiają, że niektóre zadania regex stają się prostsze. Oto jak jej użyć do liczenia wystąpień podciągu:

```java
import org.apache.commons.lang3.StringUtils;

public class CommonsLangExample {
    public static void main(String[] args) {
        String text = "Regex makes text processing easier. Processing text with regex is efficient.";
        String substring = "processing";
        
        int count = StringUtils.countMatches(text, substring);
        System.out.println("'" + substring + "' pojawia się " + count + " razy.");
    }
}
```

Aby użyć Apache Commons Lang, musisz dołączyć ją do swojego projektu. Jeśli używasz Mavena, dodaj tę zależność do swojego `pom.xml`:

```xml
<dependency>
    <groupId>org.apache.commons</groupId>
    <artifactId>commons-lang3</artifactId>
    <version>3.12.0</version> <!-- Sprawdź najnowszą wersję -->
</dependency>
```

Wyjście:
```
'processing' pojawia się 2 razy.
```
