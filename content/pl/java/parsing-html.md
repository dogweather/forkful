---
title:                "Analiza składni HTML"
html_title:           "Java: Analiza składni HTML"
simple_title:         "Analiza składni HTML"
programming_language: "Java"
category:             "Java"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/java/parsing-html.md"
---

{{< edit_this_page >}}

## Dlaczego

Masz już dość ręcznego przeglądania i kopiowania danych z stron internetowych? Chcesz poznać sposoby, jak automatycznie wyodrębnić potrzebne informacje z kodu HTML? Właśnie dlatego warto poznać parsing HTML w języku Java!

## Jak to zrobić

```Java
//importowanie bibliotek
import java.io.IOException;
import java.io.InputStream;
import java.net.URL;
import java.util.Scanner;

public class HTMLParser {

    public static void main(String[] args) {
        try {
            //pobranie kodu źródłowego strony
            URL url = new URL("https://example.com/");
            InputStream is = url.openStream();
            Scanner scanner = new Scanner(is);

            //przeszukiwanie kodu linia po linii
            while (scanner.hasNextLine()) {
                String line = scanner.nextLine();
                //wyszukiwanie wybranego tagu HTML
                if (line.contains("<h1>")) {
                    //wyodrębnianie tekstu znajdującego się pomiędzy tagami
                    String result = line.substring(line.indexOf("<h1>") + 4, line.indexOf("</h1>"));
                    System.out.println(result);
                }
            }
            scanner.close();

        } catch (IOException e) {
            e.printStackTrace();
        }
    }
}
```

Przykładowy output: 
```
Hello World!
```

## Głębszy zanurzenie

Parsing HTML polega na analizowaniu kodu HTML i wyodrębnianiu z niego potrzebnych informacji. W języku Java możemy to osiągnąć za pomocą różnych bibliotek, takich jak JSoup, HTML Parser czy TagSoup. Warto zapoznać się z ich różnicami i możliwościami, a także nauczyć się wykorzystywać XPath do precyzyjnego wyszukiwania węzłów w dokumencie HTML.

## Zobacz także

- [JSoup - biblioteka do parsowania HTML w języku Java](https://jsoup.org/)
- [HTML Parser - darmowa biblioteka do wyodrębniania danych z kodu HTML](http://htmlparser.sourceforge.net/)
- [TagSoup - narzędzie umożliwiające parsowanie niepoprawnych dokumentów HTML](https://vrici.lojban.org/~cowan/XML/tagsoup/)