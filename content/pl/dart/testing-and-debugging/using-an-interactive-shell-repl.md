---
changelog:
- 2024-03-08, OpenAIModel.GPT_4_TURBO, translated from English
date: 2024-03-08 21:56:55.995976-07:00
description: "Jak to zrobi\u0107: Dart nie posiada wbudowanego REPL. Jednak mo\u017C\
  na osi\u0105gn\u0105\u0107 funkcjonalno\u015B\u0107 podobn\u0105 do REPL, u\u017C\
  ywaj\u0105c DartPad (online) lub korzystaj\u0105c z\u2026"
lastmod: '2024-03-13T22:44:35.093218-06:00'
model: gpt-4-0125-preview
summary: Dart nie posiada wbudowanego REPL.
title: "Korzystanie z interaktywnej pow\u0142oki (REPL)"
weight: 34
---

## Jak to zrobić:
Dart nie posiada wbudowanego REPL. Jednak można osiągnąć funkcjonalność podobną do REPL, używając DartPad (online) lub korzystając z narzędzi stron trzecich, takich jak `dart_repl`.

**Korzystanie z DartPad:**

DartPad (https://dartpad.dev) to internetowy edytor Dart, który pozwala pisać i uruchamiać kod Dart w przeglądarce internetowej. Chociaż nie jest to tradycyjna powłoka poleceń REPL, zapewnia podobne doświadczenie do szybkich eksperymentów.

Po prostu wejdź na stronę, wpisz swój kod Dart w lewym panelu i kliknij "Uruchom", aby zobaczyć wynik po prawej.

Przykład:
```dart
void main() {
  print('Cześć, Dart!');
}
```
Wynik:
```
Cześć, Dart!
```

**Korzystanie z `dart_repl` (narzędzie stron trzecich):**

Najpierw zainstaluj `dart_repl` globalnie za pomocą pub:

```shell
dart pub global activate dart_repl
```

Następnie uruchom `dart_repl` z terminala:

```shell
dart_repl
```

Teraz możesz zacząć wpisywać instrukcje Dart bezpośrednio do powłoki. Na przykład:

```dart
>>> print('Cześć, REPL!');
Cześć, REPL!
>>> int add(int x, int y) => x + y;
>>> print(add(5, 7));
12
```

Te metody zapewniają szybką ścieżkę do wypróbowywania kodu Dart na bieżąco, znacznie ułatwiając krzywą uczącą się i zwiększając produktywność.
