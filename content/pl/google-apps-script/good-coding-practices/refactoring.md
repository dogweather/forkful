---
aliases:
- /pl/google-apps-script/refactoring/
changelog:
- 2024-02-01, gpt-4-0125-preview, translated from English
date: 2024-02-01 22:00:00.160115-07:00
description: "Refaktoryzacja w leksykonie programowania odnosi si\u0119 do procesu\
  \ restrukturyzacji istniej\u0105cego kodu komputerowego \u2013 zmiany faktoryzacji\
  \ bez zmiany jego\u2026"
lastmod: 2024-02-18 23:08:49.157454
model: gpt-4-0125-preview
summary: "Refaktoryzacja w leksykonie programowania odnosi si\u0119 do procesu restrukturyzacji\
  \ istniej\u0105cego kodu komputerowego \u2013 zmiany faktoryzacji bez zmiany jego\u2026"
title: Refaktoryzacja
---

{{< edit_this_page >}}

## Co i dlaczego?

Refaktoryzacja w leksykonie programowania odnosi się do procesu restrukturyzacji istniejącego kodu komputerowego – zmiany faktoryzacji bez zmiany jego zewnętrznego zachowania – w celu poprawy atrybutów niefunkcjonalnych. Jest to kluczowy krok dla programistów w celu zwiększenia czytelności kodu, zmniejszenia złożoności i potencjalnego wydobycia ukrytych błędów, ułatwiający utrzymanie i skalowalność kodu w przyszłości.

## Jak:

W Google Apps Script, typowym scenariuszem, który korzysta z refaktoryzacji, jest uproszczenie zawiłych skryptów, które współdziałają z Google Sheets lub Docs. Początkowo skrypty mogą być napisane w sposób szybki i brudny, aby szybko uzyskać wyniki. Z biegiem czasu, gdy skrypt rośnie, staje się on nieporęczny. Przeanalizujmy przykład refaktoryzacji dla lepszej czytelności i wydajności.

**Oryginalny skrypt:**

```javascript
function logSheetNames() {
  var sheets = SpreadsheetApp.getActiveSpreadsheet().getSheets();
  for (var i = 0; i < sheets.length; i++) {
    Logger.log(sheets[i].getName());
  }
}
```

Ta funkcja rejestruje nazwę każdego arkusza w Google Spreadsheet. Chociaż działa poprawnie, wykorzystuje przestarzałe praktyki JavaScript i brakuje jej klarowności.

**Zrefaktoryzowany skrypt:**

```javascript
function logSheetNames() {
  const sheets = SpreadsheetApp.getActiveSpreadsheet().getSheets();
  sheets.forEach(sheet => Logger.log(sheet.getName()));
}
```

W zrefaktoryzowanej wersji, przełączyliśmy się na używanie `const` dla zmiennych, które się nie zmieniają, czyniąc nasz zamiar bardziej zrozumiałym. Skorzystaliśmy również z metody `forEach`, bardziej nowoczesnego i zwięzłego podejścia do iteracji po tablicach, zwiększając czytelność.

**Przykładowe wyjście (dla obu skryptów):**

Wyjście w Loggerze będzie wyglądać mniej więcej tak, zakładając, że twoje dokumenty Google Sheets mają dwa arkusze nazwane "Wydatki" i "Przychody":

```
[20-04-2023 10:00:00: INFO] Wydatki
[20-04-2023 10:00:01: INFO] Przychody
```

Zrefaktoryzowany skrypt osiąga ten sam wynik, ale jest czystszy i łatwiejszy do zrozumienia na pierwszy rzut oka.

## Uwagi szczegółowe

Refaktoryzacja w Google Apps Script częściowo dziedziczy swoje zasady z szeroko pojętej praktyki inżynierii oprogramowania. Została bardziej rozpoznawalna i ustrukturyzowana jako koncepcja pod koniec lat 90-tych, szczególnie dzięki książce Martina Fowlera "Refaktoryzacja. Ulepszanie struktury istniejącego kodu" (1999), która dostarczyła obszerny przewodnik po różnych technikach refaktoryzacji. Chociaż specyfika refaktoryzacji może się różnić w zależności od języka programowania ze względu na ich składniowe i funkcjonalne różnice, główny cel pozostaje ten sam: poprawa kodu bez zmiany jego zewnętrznego zachowania.

W kontekście Google Apps Script, kluczowym aspektem do rozważenia podczas refaktoryzacji są kwoty i ograniczenia narzucone przez Google. Efektywnie zrefaktoryzowany kod nie tylko lepiej się czyta, ale także działa szybciej i bardziej niezawodnie w ramach tych ograniczeń. Na przykład, operacje wsadowe (`Range.setValues()` zamiast ustawiania wartości jedna komórka na raz) mogą znacznie zmniejszyć czas wykonania i zużycie kwot.

Warto jednak zauważyć, że dla pewnych złożonych projektów, Google Apps Script może okazać się niewystarczające ze względu na te właśnie ograniczenia. W takich przypadkach, rozważenie alternatyw takich jak Google Cloud Functions czy nowsze rodzeństwo Apps Script, AppSheet, może zaoferować lepszą skalowalność i funkcjonalność.

Ostatecznie, chociaż refaktoryzacja jest kluczową umiejętnością w utrzymaniu i ulepszaniu projektów Google Apps Script, zrozumienie ograniczeń środowiska i rozważenie alternatywnych rozwiązań jest równie ważne dla dostarczania efektywnego, solidnego i łatwego w utrzymaniu kodu.
