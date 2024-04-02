---
changelog:
- 2024-02-01, gpt-4-0125-preview, translated from English
date: 2024-02-01 22:08:56.272841-07:00
description: "Pisanie test\xF3w w Google Apps Script (GAS) polega na tworzeniu automatycznych\
  \ skrypt\xF3w, kt\xF3re weryfikuj\u0105 zachowanie Twoich kod\xF3w, zapewniaj\u0105\
  c ich\u2026"
lastmod: '2024-03-13T22:44:34.906909-06:00'
model: gpt-4-0125-preview
summary: "Pisanie test\xF3w w Google Apps Script (GAS) polega na tworzeniu automatycznych\
  \ skrypt\xF3w, kt\xF3re weryfikuj\u0105 zachowanie Twoich kod\xF3w, zapewniaj\u0105\
  c ich\u2026"
title: "Pisanie test\xF3w"
weight: 36
---

## Co i dlaczego?

Pisanie testów w Google Apps Script (GAS) polega na tworzeniu automatycznych skryptów, które weryfikują zachowanie Twoich kodów, zapewniając ich prawidłowe działanie w różnych warunkach. Programiści robią to, aby wcześnie wykrywać błędy, poprawiać jakość kodu oraz ułatwiać aktualizacje i konserwację.

## Jak to zrobić:

Chociaż Google Apps Script nie posiada wbudowanego frameworka do testowania, tak jak niektóre inne środowiska programistyczne, nadal możesz pisać i uruchamiać testy, wykorzystując proste funkcje GAS lub integrując zewnętrzne biblioteki do testowania, takie jak `QUnit`. Oto podstawowy przykład użycia prostej funkcji GAS do testowania innej funkcji w Twoim skrypcie:

```javascript
function add(a, b) {
  return a + b;
}

function testAdd() {
  var result = add(2, 3);
  if (result !== 5) {
    throw new Error("Test nieudany: add(2, 3) powinno być 5, a było " + result);
  } else {
    Logger.log("Test udany!");
  }
}
```

Uruchomienie `testAdd()` zaloguje "Test udany!", jeśli funkcja `add` działa poprawnie, lub zwróci błąd, jeśli nie. Dla bardziej zaawansowanego podejścia, integracja QUnit z Google Apps Script wymaga kilku dodatkowych kroków, ale oferuje potężne środowisko testowe. Przykładowa konfiguracja testu QUnit wygląda tak:

1. Dołącz bibliotekę QUnit do swojego projektu.
2. Utwórz plik HTML do uruchamiania testów QUnit.
3. Napisz przypadki testowe, korzystając ze składni QUnit.

Oto przykład użycia QUnit:

```javascript
// Dołącz QUnit, linkując do niego w pliku HTML używanym do uruchamiania testów

QUnit.test("Testowanie funkcji add", function (assert) {
  var result = add(2, 3);
  assert.equal(result, 5, "add(2, 3) powinno zwrócić 5");
});
```

Aby zobaczyć wyniki, otwórz plik HTML w Edytorze Skryptów GAS lub wdroż go jako aplikację internetową.

## Pogłębiona analiza

Historycznie rzecz biorąc, testowanie w Google Apps Script było do pewnego stopnia pomijane, prawdopodobnie ze względu na pochodzenie platformy i główne przypadki użycia skupiające się na szybkich, małoskalowych zadaniach automatyzacji, a nie na dużych aplikacjach. W związku z tym, GAS nie oferuje tych samych rozbudowanych frameworków i narzędzi do testowania, które znajdują się w bardziej tradycyjnych środowiskach programistycznych. Jednakże, społeczność dostosowała się, włączając biblioteki open-source i kreatywnie wykorzystując istniejące narzędzia Google.

Użycie bibliotek takich jak QUnit stanowi znaczący krok naprzód, ale wiąże się z własnym zestawem wyzwań, takich jak konfiguracja odpowiedniego środowiska testowego i nauka dodatkowej składni. Jednakże, dla tych, którzy są zainteresowani budowaniem bardziej złożonych i niezawodnych aplikacji z GAS, wysiłek ten jest wart zachodu.

Alternatywy, takie jak używanie prostych funkcji GAS do testowania, oferują łatwość użytkowania i integrację ze środowiskiem GAS bez dodatkowych zależności, ale brak im kompleksowych funkcji testowania i możliwości łatwego skalowania wraz z rozwojem projektu. Narzędzia takie jak clasp (Google Apps Script Command Line Interface) mogą ułatwić bardziej zaawansowane przepływy pracy, w tym testowanie, pozwalając programistom kodować w preferowanych IDE, co wprowadza możliwość bardziej płynnej integracji z zewnętrznymi frameworkami testowymi.

Podsumowując, chociaż GAS może nie mieć natywnej obsługi zaawansowanego testowania od razu, jego elastyczność i innowacyjne podejścia społeczności zapewniają realne ścieżki, aby zapewnić, że Twoje skrypty są solidne, niezawodne i gotowe na każde zadanie.
