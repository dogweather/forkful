---
title:                "Refaktoryzacja"
date:                  2024-01-26T01:18:34.732716-07:00
model:                 gpt-4-0125-preview
simple_title:         "Refaktoryzacja"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Good Coding Practices"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/gleam/refactoring.md"
---

{{< edit_this_page >}}

## Co i dlaczego?
Refaktoryzacja to proces przeprojektowania kodu w celu uczynienia go bardziej przejrzystym, łatwiejszym w utrzymaniu, bez zmieniania jego zewnętrznego zachowania. Programiści stosują refaktoryzację, aby poprawić czytelność, zmniejszyć złożoność oraz sprawić, że baza kodu staje się bardziej przyjazna dla przyszłych aktualizacji lub dodawania nowych funkcji.

## Jak to zrobić:
Załóżmy, że masz fragment kodu, w którym wykonujesz pewne powtarzające się obliczenia lub manipulacje łańcuchami znaków w wielu funkcjach. To doskonały cel do refaktoryzacji. Oto przykład przed i po użyciu Gleam, który kładzie duży nacisk na bezpieczeństwo typów i niezmienność:

```gleam
// Przed refaktoryzacją
pub fn calculate_area(width: Int, height: Int) -> Int {
  width * height
}

pub fn print_area(width: Int, height: Int) {
  let area = calculate_area(width, height)
  io.println("Powierzchnia wynosi \(area)")
}

// Po refaktoryzacji
pub fn calculate_area(width: Int, height: Int) -> Int {
  width * height
}

pub fn print_area(area: Int) {
  io.println("Powierzchnia wynosi \(area)")
}

// W innej części kodu, wywołasz print_area tak:
print_area(calculate_area(10, 20))
```

Przykładowe wyjście:
```
Powierzchnia wynosi 200
```

Dzięki refaktoryzacji sprawiliśmy, że `print_area` koncentruje się tylko na wydrukowaniu wyniku, podczas gdy obliczenia są przeprowadzane gdzie indziej, co czyni kod bardziej modułowym i łatwiejszym do ponownego użycia lub testowania.

## Dogłębna analiza
Refaktoryzacja jako pojęcie istnieje tak długo, jak samo programowanie — ponowne odwiedzanie i sprzątanie kodu to część dobrej gospodarki. Współczesna formalizacja refaktoryzacji, wraz z wieloma z dzisiejszych technik i wzorców, można prześledzić do przełomowej książki Martina Fowlera "Refaktoryzacja: Ulepszanie struktury istniejącego kodu" opublikowanej w 1999 roku.

W ekosystemie Gleam refaktoryzacja ma swoje specyficzne rozważania. Jednym z najważniejszych jest silna kontrola typów w czasie kompilacji, która może pomóc wcześnie wykryć błędy, gdy przenosisz elementy kodu. Dopełnianie wzorców i niezmienność w Gleam mogą również prowadzić do pisania jaśniejszego, bardziej zwięzłego kodu — jednego z głównych celów refaktoryzacji.

Alternatywami dla refaktoryzacji mogą być pisania kodu od nowa lub łatanie kodu szybkimi poprawkami. Refaktoryzacja jest jednak zwykle najbezpieczniejszym i najbardziej efektywnym podejściem do ulepszania istniejącego kodu bez wprowadzania nowych błędów, ponieważ obejmuje stopniowe, dobrze podkreślone, zachowujące zachowanie transformacje.

## Zobacz również
- Książka Martina Fowlera "Refaktoryzacja": https://martinfowler.com/books/refactoring.html
- Strona języka Gleam, zawierająca dodatkową dokumentację i przykłady: https://gleam.run/
- "Refaktoryzacja: Ulepszanie struktury istniejącego kodu" autorstwa Martina Fowlera (dla zasad mających zastosowanie w różnych językach): https://martinfowler.com/books/refactoring.html