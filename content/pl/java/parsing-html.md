---
title:                "Java: Analiza html"
simple_title:         "Analiza html"
programming_language: "Java"
category:             "Java"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/java/parsing-html.md"
---

{{< edit_this_page >}}

## Dlaczego

Jeśli jesteś programistą Javy, na pewno słyszałeś o parsowaniu HTML. Ale czy wiesz dlaczego warto się za to zabrać? Parsowanie HTML jest niezbędnym narzędziem dla tworzenia różnego rodzaju aplikacji internetowych, w szczególności tych opartych na web scrapingu. Dzięki parsowaniu HTML możesz w prosty sposób pobierać informacje z witryn internetowych i wykorzystywać je w swoim kodzie.

## Jak to zrobić

Aby wykonać parsowanie HTML za pomocą Javy, musimy najpierw skorzystać z narzędzia o nazwie JSoup. Jest to niesamowicie użyteczna biblioteka, która pozwala nam przetwarzać i pobierać dane z dokumentów HTML. W poniższym przykładzie użyjemy JSoup do pobrania tytułu artykułu z wybranej strony internetowej.

```Java
import org.jsoup.Jsoup; 
import org.jsoup.nodes.Document;

Document doc = Jsoup.connect("https://example.com").get();

String title = doc.title();
System.out.println(title); // Wypisze "Przykładowa strona internetowa"
```

W powyższym kodzie najpierw musimy zaimportować bibliotekę JSoup, a następnie utworzyć obiekt Document dla wybranej strony internetowej. Następnie możemy wykorzystać metodę `title()` do pobrania tytułu strony i wyświetlić go na ekranie.

## Głębszy zanurzenie

Parsowanie HTML może być jednak bardziej złożonym procesem niż wskazuje na to prosty przykład powyżej. Często musimy wykonać wiele kroków, aby uzyskać żądane dane. Na szczęście JSoup pozwala na użycie selektorów CSS do wybierania odpowiednich elementów HTML, co ułatwia nam zadanie. Możemy również korzystać z metod `getElementsByClass()` lub `getElementsByTag()` do pobierania elementów po klasie lub tagu.

Ponadto, jeśli zdecydujemy się na bardziej zaawansowane parsowanie, możemy także wykorzystać reguły XPath. Dzięki temu będziemy mogli precyzyjniej określić, które elementy chcemy pobrać z dokumentu HTML.

## Zobacz także

Jeśli chcesz dowiedzieć się więcej o parsowaniu HTML z wykorzystaniem Javy, polecamy zapoznanie się z poniższymi artykułami:

- [Tutoriale o JSoup](https://jsoup.org/cookbook/)
- [Oficjalna dokumentacja JSoup](https://jsoup.org/apidocs/)
- [Przykładowe projekty wykorzystujące parsowanie HTML](https://github.com/topics/jsoup) (Github)