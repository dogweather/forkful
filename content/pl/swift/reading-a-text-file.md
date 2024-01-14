---
title:                "Swift: Czytanie pliku tekstowego"
programming_language: "Swift"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/swift/reading-a-text-file.md"
---

{{< edit_this_page >}}

## Dlaczego

Przeczytanie pliku tekstowego może wydawać się banalnym zadaniem, ale jest to podstawowa umiejętność, która jest niezbędna w świecie programowania. Bez możliwości odczytania i przetwarzania tekstu, trudno byłoby stworzyć aplikacje, które są użyteczne i wykonują konkretne zadania. Dlatego warto poświęcić czas na naukę odczytywania plików tekstowych.

## Jak to zrobić

Do odczytania pliku tekstowego w języku Swift wykorzystuje się klasę FileManager oraz funkcję Data(contentsOf:). Poniżej przedstawiony jest przykładowy kod, który pokazuje jak odczytać plik tekstowy w formacie CSV.

```Swift
if let path = Bundle.main.path(forResource: "plik", ofType: "csv") {
    do {
        let data = try Data(contentsOf: URL(fileURLWithPath: path))
        if let contents = String(data: data, encoding: .utf8) {
            print(contents)
        }
    } catch {
        print(error)
    }
}
```

Powyższy kod wykorzystuje Bundle, aby uzyskać ścieżkę do pliku w aplikacji, a następnie odczytuje zawartość pliku przy użyciu funkcji Data. Następnie zawartość pliku jest przetwarzana na String i wyświetlana na konsoli.

## Głębsza analiza

Odczytanie pliku tekstowego może być nieco bardziej skomplikowane, jeśli chcemy uwzględnić specyficzne formatowania, takie jak znaki nowej linii lub tabulatory. W takim przypadku, należy użyć klas dostępnych w języku Swift do manipulacji tekstem, takich jak NSString lub NSStringEncoding.

Inną ważną rzeczą do zapamiętania podczas odczytywania pliku tekstowego jest właściwe obsłużenie błędów. Wielu początkujących programistów często pomija tę kwestię, ale jest to ważne dla uniknięcia nieoczekiwanych problemów w przyszłości.

## Zobacz także

- Dokumentacja Apple na temat FileManager: <https://developer.apple.com/documentation/foundation/filemanager>
- Dokumentacja Apple na temat funkcji Data(contentsOf:): <https://developer.apple.com/documentation/foundation/data/3247231-init>
- Przewodnik po języku Swift: <https://swift.org/documentation/>