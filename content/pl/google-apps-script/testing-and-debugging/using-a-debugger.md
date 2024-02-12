---
title:                "Korzystanie z debuggera"
aliases:
- pl/google-apps-script/using-a-debugger.md
date:                  2024-02-01T22:03:16.732987-07:00
model:                 gpt-4-0125-preview
simple_title:         "Korzystanie z debuggera"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/google-apps-script/using-a-debugger.md"
changelog:
  - 2024-02-01, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Co i dlaczego?

Debugowanie w Google Apps Script (GAS) polega na identyfikacji i usuwaniu błędów ze skryptów mających na celu automatyzację Google Apps lub budowę aplikacji internetowych. Programiści debugują, aby upewnić się, że ich kod wykonuje się zgodnie z oczekiwaniami, zwiększając niezawodność i wydajność aplikacji.

## Jak to zrobić:

Google Apps Script zapewnia wbudowany debugger w Edytorze Apps Script, pomagając w rozwiązywaniu problemów ze skryptami. Oto, jak zainicjować i używać debugera:

1. **Otwórz swój skrypt w Edytorze Apps Script.**
2. **Wybierz funkcję do debugowania.** Z menu rozwijanego na górze wybierz funkcję, którą chcesz zdebugować.
3. **Ustaw punkty przerwania.** Kliknij na marginesie (szary obszar po lewej stronie numerów linii), gdzie chcesz wstrzymać wykonanie; pojawi się czerwona kropka, oznaczająca punkt przerwania.
4. **Rozpocznij debugowanie.** Kliknij na ikonę błędu lub wybierz `Debugowanie` > `Rozpocznij debugowanie`. Wykonanie rozpocznie się i zatrzyma na pierwszym punkcie przerwania.

Rozważ ten prosty skrypt:

```javascript
function calculateSum() {
  var a = 5;
  var b = 10;
  var sum = a + b;
  Logger.log(sum); // Zamierzone do zalogowania 15
}
```

Jeśli nie jesteś pewny, dlaczego `Logger.log(sum)` nie wyświetla oczekiwanego wyniku, możesz ustawić punkt przerwania w linii `var sum = a + b;` i przejść przez skrypt linia po linii, aby zbadać wartości zmiennych.

**Przykładowe wyjście w Logger:**

```plain
15
```

Podczas debugowania, Edytor Apps Script umożliwia:

- **Przechodzenie przez kod** przy użyciu przycisków krok dalej, krok do wewnątrz i krok na zewnątrz.
- **Obserwowanie wyrażeń i zmiennych** aby zobaczyć ich wartości zmieniające się w czasie rzeczywistym.
- **Inspekcję stosu wywołań** aby śledzić wywołania funkcji.

## Wgłębiając się

Debugowanie w Google Apps Script, podobnie jak w każdym innym środowisku programistycznym, jest niezbędne do tworzenia aplikacji wolnych od błędów. Wprowadzony na wczesnym etapie rozwoju GAS, wbudowany debugger oferuje podstawowe możliwości inspekcji i naprawiania kodu krok po kroku. Chociaż zapewnia podstawowe funkcje debugowania podobne do tych znalezionych w bardziej dojrzałych środowiskach takich jak Visual Studio Code czy IntelliJ, może być niewystarczający dla skomplikowanych scenariuszy debugowania. Na przykład, jego możliwości inspekcji asynchronicznych wywołań zwrotnych lub zarządzania ciężkimi wykonaniami skryptów mogą być ograniczone.

Dla skomplikowanych potrzeb debugowania programiści mogą uciekać się do alternatywnych metod takich jak obszerne logowanie (używając `Logger.log()`) lub nawet wdrażanie jako aplikacja webowa do inspekcji zachowania w scenariuszu rzeczywistym. Jednakże, prostota i integracja debugera GAS w Edytorze Apps Script czynią go bezcennym pierwszym krokiem do rozwiązywania problemów i zrozumienia zachowania skryptu. Co godne uwagi, z ciągłymi aktualizacjami i ulepszeniami Google dotyczącymi Apps Script, doświadczenie z debugowaniem stale się poprawia, oferując bardziej zaawansowane narzędzia i opcje w miarę upływu czasu. Ta ewolucja odzwierciedla zaangażowanie Google w uczynienie Apps Script bardziej potężną i dostępną platformą dla programistów z różnorodnymi tłem.
