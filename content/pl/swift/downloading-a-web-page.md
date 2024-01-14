---
title:                "Swift: Pobieranie strony internetowej"
simple_title:         "Pobieranie strony internetowej"
programming_language: "Swift"
category:             "Swift"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/swift/downloading-a-web-page.md"
---

{{< edit_this_page >}}

## Dlaczego

Pobieranie stron internetowych jest niezbędnym elementem programowania w dzisiejszych czasach. Dzięki temu możemy tworzyć aplikacje, które wykorzystują dane z internetu, takie jak aktualności, informacje o pogodzie czy też wyniki meczów. Bez pobierania stron, nasze aplikacje byłyby ograniczone do danych tylko dostępnych w nich samych.

## Jak to zrobić

Pobranie strony internetowej w języku Swift jest bardzo proste. Wystarczy zastosować kilka linijek kodu, a już będziemy w stanie pobrać stronę i wyświetlić jej zawartość. Poniżej przedstawiam przykładowy kod oraz jego rezultat:

```Swift
let url = URL(string: "https://www.example.com") // utworzenie URL dla wybranej strony
if let data = try? Data(contentsOf: url!) { // pobranie danych ze strony
    let htmlData = NSString(data: data, encoding: String.Encoding.utf8.rawValue) // konwersja danych z binarnych na tekst HTML
    print(htmlData as Any) // wyświetlenie zawartości strony
}
```

Rezultat:

```
<html> 
  <head> ... </head>
  <body> ... </body>
</html>
```

Dzięki temu prostemu kodowi, możemy pobierać i wykorzystywać dane z dowolnej strony internetowej.

## Głębsza analiza

Pobieranie stron internetowych w języku Swift może być także ulepszone poprzez wykorzystanie biblioteki Alamofire, która oferuje łatwiejsze i bardziej przejrzyste sposoby pobierania danych z internetu. Dodatkowo, można również wykorzystać narzędzie HTMLParser, aby przeanalizować i wyciągnąć konkretne elementy z pobranej strony.

## Zobacz też

- [Dokumentacja Swift: Foundation](https://developer.apple.com/documentation/foundation)
- [Alamofire](https://github.com/Alamofire/Alamofire)
- [HTMLParser](https://github.com/tid-kijyun/Kanna)