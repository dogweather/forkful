---
title:                "Generowanie liczb losowych"
aliases: - /pl/vba/generating-random-numbers.md
date:                  2024-02-01T21:55:43.855562-07:00
model:                 gpt-4-0125-preview
simple_title:         "Generowanie liczb losowych"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/vba/generating-random-numbers.md"
changelog:
  - 2024-02-01, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Co i dlaczego?

Generowanie losowych liczb w Visual Basic for Applications (VBA) umożliwia programom symulowanie procesów z elementami szansy lub zmienności, takimi jak rzuty kostką czy próbkowanie danych. Programiści wykorzystują te techniki do tworzenia modeli, gier lub symulacji, gdzie przewidywalne wyniki byłyby nierealistyczne lub mniej użyteczne.

## Jak to zrobić:

W VBA funkcję `Rnd` używa się do generowania losowych liczb. Domyślnie `Rnd` generuje pojedynczą liczbę zmiennoprzecinkową o precyzji pojedynczej większą lub równą 0 i mniejszą niż 1. Oto kilka kroków i przykładów, jak efektywnie korzystać z losowych numerów:

1. **Prosta losowa liczba:**
   Aby wygenerować podstawową losową liczbę, wystarczy wywołać `Rnd()`:

   ```vb
   Sub GenerateRandomNumber()
       Dim randomNumber As Single
       randomNumber = Rnd() ' Losowa liczba między 0 a 1
       MsgBox randomNumber
   End Sub
   ```

2. **Ustawianie ziarna:**
   Instrukcja `Randomize` inicjuje generator liczb losowych, co może być kluczowe dla zapewnienia różnych wyników za każdym razem, gdy kod VBA jest uruchamiany:

   ```vb
   Sub SeedRandomNumber()
       Randomize
       Dim randomNumber As Single
       randomNumber = Rnd()
       MsgBox randomNumber
   End Sub
   ```

3. **Generowanie liczb w zakresie:**
   Często potrzebna jest losowa liczba w określonym zakresie. Oto jak wygenerować liczbę między 1 a 100:

   ```vb
   Sub RandomNumberInRange()
       Randomize
       Dim randomNumber As Integer
       randomNumber = Int((100 * Rnd()) + 1) ' Losowa liczba między 1 a 100
       MsgBox randomNumber
   End Sub
   ```

### Przykładowy wynik:
Po uruchomieniu `RandomNumberInRange`, możesz zobaczyć okno komunikatu wyświetlające liczbę taką jak `45`.

## Szczegółowe omówienie:

Funkcja `Rnd` w VBA, choć łatwa w użyciu, faktycznie generuje pseudolosowe liczby oparte na deterministycznym algorytmie. Oznacza to, że sekwencje liczb, które produkuje, nie są naprawdę losowe, ale często wystarczają dla wspólnych zadań wymagających procesów stochastycznych.

Historycznie, możliwość generowania liczb losowych w VBA sięga wczesnych wersji Basic, dostosowując się z czasem do cech takich jak `Randomize` w celu poprawy losowości poprzez ziarnowanie algorytmu punktem startowym. Jednakże, dla aplikacji wymagających wysokich poziomów losowości, takich jak bezpieczne operacje kryptograficzne, funkcja `Rnd` w VBA może nie być najlepszym narzędziem. Alternatywy w bardziej rozbudowanych środowiskach programistycznych lub językach zaprojektowanych z myślą o kryptografii, takie jak moduł `secrets` w Pythonie czy `SecureRandom` w Javie, powinny być rozważane.

Pomimo swoich ograniczeń, prostota i dostępność generowania losowych liczb w VBA nadal czynią to narzędzie wartościowym dla szerokiego zakresu lżejszych aplikacji, prac symulacyjnych i celów edukacyjnych.
