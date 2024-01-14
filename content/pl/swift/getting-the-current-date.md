---
title:                "Swift: Otrzymywanie bieżącej daty"
simple_title:         "Otrzymywanie bieżącej daty"
programming_language: "Swift"
category:             "Swift"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/swift/getting-the-current-date.md"
---

{{< edit_this_page >}}

## Dlaczego? 

W dzisiejszych czasach, programowanie stało się nieodłącznym elementem naszego życia. Wiele osób codziennie korzysta z różnego rodzaju aplikacji i programów, a przy tym nie zdaje sobie sprawy z tego, ile pracy i algorytmów stoi za ich działaniem. Jeśli jesteś jedną z tych osób, które chcą zacząć swoją przygodę z programowaniem, lub po prostu chcesz dowiedzieć się więcej na temat jednej z podstawowych funkcji języka Swift - pobierania aktualnej daty, to ten artykuł jest dla Ciebie.

## Jak to zrobić?

Aby pobrać aktualną datę w języku Swift, możemy skorzystać z klasy `Date`. Jest to klasa, która reprezentuje datę i czas w naszym kodzie. Przejdźmy teraz do praktyki i zobaczmy, jak wykorzystać tę klasę w naszym kodzie.

```Swift
// Tworzymy obiekt klasy Date przechowujący aktualną datę i czas
let currentDate = Date()
// Tworzymy obiekt klasy DateFormatter, który posłuży nam do formatowania daty
let dateFormatter = DateFormatter()
// Ustawiamy format daty, w którym chcemy wyświetlać dane
dateFormatter.dateFormat = "dd/MM/yyyy"
// Wykorzystujemy date formatter do wyświetlenia aktualnej daty w wybranym przez nas formacie
print(dateFormatter.string(from: currentDate))
// Output: 10/02/2021
```

Powyższy przykład pokazuje, jak prostym sposobem możemy pobrać i wyświetlić aktualną datę w wybranym przez nas formacie. Oczywiście, format możemy dostosować do swoich potrzeb, wykorzystując dostępne specyfikatory daty.

## Głębsze wgląd

Teraz, gdy już wiemy, jak pobierać i formatować datę w języku Swift, warto poznać jeszcze kilka istotnych informacji na jej temat. Należy pamiętać, że obiekt `Date` przechowuje datę i czas w określonej strefie czasowej. Domyślnie, jest to strefa czasowa urządzenia, na którym nasz kod jest wykonywany. W przypadku potrzeby pracy z inną strefą czasową, możemy wykorzystać metodę `DateFormatter` `setTimeZone(_:)` i ustawić odpowiednią strefę czasową.

Kolejną ważną klasą jest `Calendar`, która pozwala nam na wykonywanie operacji na dacie, takich jak dodawanie lub odejmowanie określonej liczby jednostek czasu. Warto zaznajomić się również z metodami takimi jak `dateComponents(_:, from:)` czy `dateInterval(of:for:)`, które pozwalają na wydobycie lub ustawienie konkretnych informacji o dacie.

## Zobacz także

1. Dokumentacja Apple na temat klasy [Date](https://developer.apple.com/documentation/foundation/date)
2. Przewodnik po formatowaniu daty i czasu w języku Swift [https://nshipster.com/dateformatter/](https://nshipster.com/dateformatter/)
3. Przykładowy projekt wykorzystujący klasy `Date` i `Calendar` [https://github.com/vprazz/swift-date-app](https://github.com/vprazz/swift-date-app)

Dzięki temu artykułowi, już teraz wiesz jak pobierać aktualną datę w języku Swift, jak możesz ją formatować oraz jak wykonywać operacje na dacie. Teraz wystarczy wykorzystać tę wiedzę w swoich projektach i cieszyć się prostotą i skutecznością tej funkcji. Powodzenia!