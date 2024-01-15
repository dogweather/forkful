---
title:                "Analiza składni HTML"
html_title:           "C#: Analiza składni HTML"
simple_title:         "Analiza składni HTML"
programming_language: "C#"
category:             "C#"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/c-sharp/parsing-html.md"
---

{{< edit_this_page >}}

## Dlaczego

Każdy, kto pracuje z internetem, ma do czynienia z dokumentami HTML. Może to być tworzenie stron internetowych, scrapowanie danych lub wykorzystanie informacji dostarczonych przez serwisy internetowe. W takich przypadkach, rozumienie i analiza HTML jest niezbędna, a programowanie parsera dla tego języka może ułatwić wiele zadań.

## Jak to zrobić

Ważnym elementem parsowania HTML jest korzystanie z biblioteki lub narzędzia, które ułatwiają ten proces. W przypadku języka C#, jednym z popularnych wyborów jest biblioteka HtmlAgilityPack.

```C#
// instancja klasy HtmlAgilityPack.HtmlDocument
HtmlDocument doc = new HtmlDocument();

// załadowanie zawartości HTML
doc.Load("strona.html");

// wybieranie elementów na podstawie tagu
HtmlNodeCollection divs = doc.DocumentNode.SelectNodes("//div");

// wyświetlenie tekstu z wybranego elementu
Console.WriteLine(divs[0].InnerText);
```

Kod powyżej pokazuje podstawową funkcjonalność parsera HtmlAgilityPack. Za pomocą metody `SelectNodes()` możemy wybierać elementy na podstawie tagów, a następnie korzystając ze właściwości `InnerText` uzyskać zawartość tekstu.

## Wnikliwsza analiza

Istnieje wiele innych metod i właściwości, które można wykorzystać przy parsowaniu HTML. Na przykład, można korzystać z xpath do precyzyjnego wybierania elementów lub wybierać elementy na podstawie ich klas i identyfikatorów. Dodatkowo, znając strukturę i składnię HTML, można ustawić właściwości biblioteki w sposób, który ułatwi zrozumienie i analizę dokumentu.

## Zobacz również

1. [Dokumentacja biblioteki HtmlAgilityPack](https://html-agility-pack.net/documentation)
2. [Kurs C# dla początkujących](https://www.youtube.com/playlist?list=PLJeTUJQAsfMIqkDq_8VSIpKTm5Z_LNbjh)
3. [Tutorial o wyrażeniach xpath](https://www.w3schools.com/xml/xpath_intro.asp)