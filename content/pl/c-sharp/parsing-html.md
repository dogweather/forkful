---
title:                "C#: Odczytywanie html"
simple_title:         "Odczytywanie html"
programming_language: "C#"
category:             "C#"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/c-sharp/parsing-html.md"
---

{{< edit_this_page >}}

## Dlaczego?

Przetwarzanie HTML jest nieodłącznym elementem wielu projektów programistycznych, ponieważ pozwala na wydobycie informacji z witryn internetowych. Jest to szczególnie przydatne przy tworzeniu aplikacji, które polegają na pobieraniu i analizowaniu danych z różnych stron internetowych. W tym artykule dowiecie się, jak w łatwy sposób wdrożyć tę funkcjonalność w języku C#.

## Jak to zrobić?

Aby parsować HTML w C#, wystarczy wykorzystać bibliotekę HtmlAgilityPack. Najpierw musisz zainstalować tę bibliotekę za pomocą menedżera pakietów NuGet. Następnie zaimportuj odpowiednie biblioteki do swojego projektu, aby móc korzystać z jej funkcji. Przykładowy kod wyglądałby następująco:

```C#
var web = new HtmlAgilityPack.HtmlWeb();
var document = web.Load("https://www.example.com");
var title = document.DocumentNode.SelectSingleNode("//title");
Console.WriteLine(title.InnerText);
```

Ten kod przeprowadziłby parsowanie tytułu strony internetowej i wyświetlił go w konsoli. Możesz również wykorzystać różne metody, takie jak `SelectNodes` lub `GetAttributeValue`, aby wydobyć inne elementy lub atrybuty z wybranej witryny.

## Głębsze wgląd

Biblioteka HtmlAgilityPack umożliwia również korzystanie z CSS selektorów do łatwego przeszukiwania drzewa dokumentu HTML. W ten sposób możesz znaleźć i wydobyć konkretne elementy, takie jak nagłówki, obrazy czy linki. Możesz także wykorzystać możliwości filtrowania oraz sortowania wyników. Wszystko to pozwala na jeszcze bardziej elastyczne i precyzyjne parsowanie.

Kolejną zaletą biblioteki jest obsługa błędów i wyjątków. W przypadku, gdy pobrana strona nie jest poprawnym dokumentem HTML, możesz określić, co ma zostać zwrócone lub jak obsłużyć ten błąd.

## Zobacz również

- [HtmlAgilityPack](https://html-agility-pack.net/)
- [Dokumentacja HtmlAgilityPack](https://html-agility-pack.net/documentation)