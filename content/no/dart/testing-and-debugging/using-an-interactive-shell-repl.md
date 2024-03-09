---
title:                "Å bruke et interaktivt skall (REPL)"
date:                  2024-03-08T21:56:41.410101-07:00
model:                 gpt-4-0125-preview
changelog:
  - 2024-03-08, OpenAIModel.GPT_4_TURBO, translated from English
---

{{< edit_this_page >}}

## Hva & Hvorfor?

En interaktiv shell (REPL - Read-Evaluate-Print Loop) for Dart lar programmerere dynamisk skrive inn og kjøre Dart-kode linje for linje uten å måtte kompilere hele skript. Dette verktøyet er uvurderlig for å lære Dart-syntaks, eksperimentere med kodeutsnitt, eller feilsøking ved å tilby øyeblikkelig tilbakemelding og lette iterativ testing.

## Hvordan:

Dart kommer ikke med en innebygd REPL. Du kan imidlertid oppnå REPL-lignende funksjonalitet ved å bruke DartPad (online) eller ved å benytte tredjepartsverktøy som `dart_repl`.

**Bruke DartPad:**

DartPad (https://dartpad.dev) er en nettbasert Dart-editor som lar deg skrive og kjøre Dart-kode i nettleseren din. Selv om det ikke er en tradisjonell kommandolinje-REPL, gir det en lignende opplevelse for rask eksperimentering.

Gå enkelt til nettstedet, skriv inn Dart-koden din i venstre rute, og klikk på "Kjør" for å se resultatet på høyre side.

Eksempel:
```dart
void main() {
  print('Hallo, Dart!');
}
```
Output:
```
Hallo, Dart!
```

**Bruk av `dart_repl` (tredjepartsverktøy):**

Først, installer `dart_repl` via pub globalt:

```shell
dart pub global activate dart_repl
```

Deretter kjører du `dart_repl` fra terminalen din:

```shell
dart_repl
```

Nå kan du begynne å skrive inn Dart-setninger direkte i shellen. For eksempel:
```dart
>>> print('Hallo, REPL!');
Hallo, REPL!
>>> int add(int x, int y) => x + y;
>>> print(add(5, 7));
12
```

Disse metodene tilbyr en rask vei for å prøve ut Dart-kode på farten, noe som betydelig letter læringskurven og øker produktiviteten.
