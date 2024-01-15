---
title:                "Konwertowanie daty na ciąg znaków"
html_title:           "Swift: Konwertowanie daty na ciąg znaków"
simple_title:         "Konwertowanie daty na ciąg znaków"
programming_language: "Swift"
category:             "Swift"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/swift/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## Dlaczego

Konwertowanie daty na ciąg znaków może być niezbędnym krokiem podczas pracy z datami w aplikacjach. Może to być również przydatne przy wyświetlaniu daty w czytelnej formie dla użytkownika.

## Jak to zrobić

```Swift
let date = Date()
let formatter = DateFormatter()
formatter.dateFormat = "dd.MM.yyyy"
let dateString = formatter.string(from: date)
print(dateString) // Output: 25.01.2021
```

W powyższym przykładzie użyliśmy obiektu `Date` do utworzenia daty bieżącej, następnie zdefiniowaliśmy formater dat `DateFormatter` i ustawiliśmy format na `dd.MM.yyyy`, który reprezentuje dzień, miesiąc i rok. Następnie wykorzystaliśmy metodę `string(from: date)` by skonwertować datę na ciąg znaków, który możemy wyświetlić w konsoli.

## Dogłębne wgląd

Konwertowanie daty na ciąg znaków może być nieco bardziej złożone, jeśli chcemy włączyć w to także informacje o czasie. W takim przypadku musimy użyć formatu, który zawiera także informacje o godzinie i minucie, na przykład `"dd.MM.yyyy HH:mm"`. Istnieje wiele różnych formatów dat, które można wykorzystać podczas konwertowania daty na ciąg znaków w zależności od naszych potrzeb. Warto również pamiętać o lokalizacji i ustawieniach regionalnych, ponieważ mogą one mieć wpływ na wyświetlane daty.

## Zobacz także

- [Dokumentacja Apple: DateFormatter](https://developer.apple.com/documentation/foundation/dateformatter)
- [Przewodnik po konfiguracji formaterów dat w Swift](https://www.hackingwithswift.com/articles/145/how-to-use-date-and-dateformatter-in-swift)