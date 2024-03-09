---
title:                "Łączenie łańcuchów znaków"
date:                  2024-03-08T21:53:35.051792-07:00
model:                 gpt-4-0125-preview
changelog:
  - 2024-03-08, OpenAIModel.GPT_4_TURBO, translated from English
---

{{< edit_this_page >}}

## Co i dlaczego?
Konkatenacja łańcuchów w programowaniu polega na połączeniu dwóch lub więcej łańcuchów w jeden. Programiści robią to, aby łatwiej manipulować danymi tekstowymi, konstruować wiadomości lub dynamicznie składać części interfejsu użytkownika.

## Jak to zrobić:
Dart oferuje kilka prostych sposobów na konkatenację łańcuchów. Poniżej znajdują się najczęstsze metody:

### Użycie operatora `+`
Operator `+` jest najbardziej intuicyjnym sposobem łączenia łańcuchów.
```dart
String powitanie = 'Hello, ' + 'World!';
print(powitanie); // Wyjście: Hello, World!
```

### Użycie metody `concat()`
Chociaż Dart nie ma metody `concat()` podobnej do innych języków, osiągnięcie tego samego można zrobić za pomocą `+` lub poniższych metod.

### Użycie interpolacji łańcuchów
Interpolacja łańcuchów pozwala bezpośrednio osadzać zmienne w łańcuchu. Jest to wydajne w przypadku łączenia łańcuchów i wyrażeń.
```dart
String uzytkownik = 'Jane';
String wiadomosc = 'Welcome, $uzytkownik!';
print(wiadomosc); // Wyjście: Welcome, Jane!
```

### Użycie metody `join()`
Metoda `join()` jest przydatna, gdy masz listę łańcuchów, które chcesz połączyć.
```dart
var slowa = ['Hello', 'from', 'Dart'];
String zdanie = slowa.join(' '); // Połącz z separatorem spacji.
print(zdanie); // Wyjście: Hello from Dart
```

### Użycie StringBuffer
`StringBuffer` jest wydajny dla wielokrotnych konkatenacji, zwłaszcza w pętlach.
```dart
var slowa = ['Dart', 'jest', 'fajny'];
StringBuffer bufor = StringBuffer();
for (String slowo in slowa) {
  bufor.write(slowo); // Dodaj każde słowo do bufora.
  bufor.write(' '); // Opcjonalnie dodaj spację.
}
String zdanie = bufor.toString().trim(); // Konwertuj na łańcuch i usuń końcową spację.
print(zdanie); // Wyjście: Dart jest fajny
```

### Biblioteki osób trzecich
Chociaż standardowa biblioteka Darta zazwyczaj wystarcza do zadań związanych z konkatenacją łańcuchów, biblioteki osób trzecich, takie jak `quiver`, oferują narzędzia, które mogą uzupełniać wbudowaną funkcjonalność Darta. Na przykład funkcje `concat()` lub `merge()` z `quiver` mogą być zbadane w zaawansowanych scenariuszach. Jednak trzymaj się solidnych, wbudowanych opcji Darta, chyba że masz konkretną potrzebę, której one nie pokrywają.
