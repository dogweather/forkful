---
title:                "Zamiana liter na wielkie w ciągu znaków"
date:                  2024-01-19
html_title:           "Arduino: Zamiana liter na wielkie w ciągu znaków"
simple_title:         "Zamiana liter na wielkie w ciągu znaków"
programming_language: "Swift"
category:             "Swift"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/swift/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## Co i Dlaczego?
W Swift kapitalizacja stringów to po prostu zmiana pierwszych liter słów na wielkie. Robisz to, by ujednolicić wygląd danych lub przygotować tekst do wyświetlenia użytkownikowi.

## Jak to zrobić:
```Swift
let smallTalk = "witaj, jak się masz?"
let capitalizedTalk = smallTalk.capitalized

print(capitalizedTalk) // Wydruk: "Witaj, Jak Się Masz?"
```

## W Głąb Tematu
Kapitalizacja stringów w programowaniu nie jest nowością. We wcześniejszych językach też to robiliśmy, ale każdy język ma własne zasady i funkcje. W Swift kapitalizacja jest prosta dzięki wbudowanej funkcji `.capitalized`, która działa na każdym `String`. Alternatywnie, możesz używać `Locale` do precyzyjnej kontroli nad kapitalizacją zgodnie z konkretnymi ustawieniami lokalnymi.

Najważniejsze to pamiętać, że metoda `.capitalized` Swifta zachowuje się inaczej niż np. `toUpperCase()` w Java. Swift bierze pod uwagę punktacyjne granice słowa, zamiast tylko białych znaków. To oznacza, że każde "słowo" w zdaniu zostanie zmienione na wersję z dużą literą na początku.

```Swift
let greetingsFromDifferentLanguages = "cześć! hello! ¡hola! こんにちは!"
let titlesCase = greetingsFromDifferentLanguages.capitalized(with: Locale.current)

print(titlesCase) // Wydruk: "Cześć! Hello! ¡Hola! こんにちは!"
```

Przykład powyżej pokazuje, jak w Swift zrobić string każdego przywitania z dużych liter, zachowując przy tym oryginalne środki interpunkcyjne.

## Zobacz jeszcze
- Swift String Handling Guide: [https://www.swiftbysundell.com/basics/strings/](https://www.swiftbysundell.com/basics/strings/)
- Locale-specific capitalization: [https://developer.apple.com/documentation/foundation/nslocale](https://developer.apple.com/documentation/foundation/nslocale)
