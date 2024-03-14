---
changelog:
- 2024-02-01, gpt-4-0125-preview, translated from English
date: 2024-02-01 21:56:15.760202-07:00
description: "Rejestrowanie (logowanie) w programowaniu polega na zapisywaniu zdarze\u0144\
  , b\u0142\u0119d\xF3w lub wybitnych wyst\u0105pie\u0144 podczas dzia\u0142ania programu.\
  \ Programi\u015Bci robi\u0105 to w\u2026"
lastmod: '2024-03-13T22:44:34.910141-06:00'
model: gpt-4-0125-preview
summary: "Rejestrowanie (logowanie) w programowaniu polega na zapisywaniu zdarze\u0144\
  , b\u0142\u0119d\xF3w lub wybitnych wyst\u0105pie\u0144 podczas dzia\u0142ania programu.\
  \ Programi\u015Bci robi\u0105 to w\u2026"
title: Rejestrowanie
---

{{< edit_this_page >}}

## Co i dlaczego?

Rejestrowanie (logowanie) w programowaniu polega na zapisywaniu zdarzeń, błędów lub wybitnych wystąpień podczas działania programu. Programiści robią to w celu debugowania problemów, monitorowania wydajności i prowadzenia rejestru danych operacyjnych, co czyni to kluczowym dla utrzymania i zrozumienia zachowania oprogramowania w produkcji.

## Jak to zrobić:

W Google Apps Script logowanie można wykonać za pomocą różnych metod, takich jak klasa `Logger` oraz `console.log()`. Klasa Logger to tradycyjny sposób, odpowiedni do prostego debugowania i celów rozwojowych. W ramach ostatnich aktualizacji `console.log()` oferuje większą elastyczność i integrację ze Stackdriver Logging, zapewniając bardziej solidne rozwiązanie do monitorowania skryptów Apps Scripts w Google Cloud Platform.

**Korzystanie z Logger:**

```javascript
function logSample() {
  Logger.log('To jest prosty komunikat logowania');
  
  var value = 5;
  Logger.log('Wartość wynosi: %s', value); // Formatowanie ciągu znaków
}

// Aby wyświetlić log:
// 1. Uruchom funkcję logSample.
// 2. Widok -> Logi
```

**Przykładowy wyjściowy Logger:**

```
[22-04-20 10:00:00:000 PDT] To jest prosty komunikat logowania
[22-04-20 10:00:00:001 PDT] Wartość wynosi: 5
```

**Korzystanie z console.log():**

```javascript
function consoleLogSample() {
  console.log('Ta wiadomość trafia do Stackdriver Logging');
  const obj = {name: 'Jane', role: 'Developer'};
  console.info('Logowanie obiektu:', obj);
}

// Logi można wyświetlić w konsoli Google Cloud Platform (GCP) w sekcji Stackdriver Logging
```

**Przykładowy wyjściowy console.log():**

```
Ta wiadomość trafia do Stackdriver Logging
Logowanie obiektu: {name: "Jane", role: "Developer"}
```

Przechodząc na `console.log()` dla złożonych aplikacji, programiści mogą efektywnie przetwarzać i analizować logi, korzystając z potężnych filtrów i narzędzi dostarczanych przez GCP, co nie jest tak proste przy użyciu tradycyjnej klasy Logger.

## Pogłębiona analiza:

Logowanie w Google Apps Script znacznie ewoluowało. Początkowo klasa `Logger` była główną metodą dla programistów do debugowania ich skryptów. Jest prosta i wystarczająca dla podstawowych skryptów, ale brakuje jej zdolności potrzebnych dla nowoczesnych aplikacji chmurowych, takich jak wyszukiwanie logów czy analizowanie trendów logów w czasie.

Wprowadzenie `console.log()` zniwelowało tę lukę, integrując logowanie Google Apps Script z logowaniem Stackdriver Google Cloud (obecnie zwane Operations Suite), dostarczając scentralizowaną platformę do logowania, monitorowania i debugowania aplikacji. Nie tylko umożliwiło to logowanie na dużą skalę, ale również otworzyło zaawansowane funkcje zarządzania logami, takie jak metryki oparte na logach, analiza logów w czasie rzeczywistym i integracja z innymi usługami Google Cloud.

Chociaż `Logger` nadal pełni rolę do szybkiego debugowania i logowania w mniejszych skryptach, ewolucja w kierunku używania `console.log()` odzwierciedla szersze przejście do tworzenia skalowalnych, natywnych aplikacji chmurowych. Podkreśla to zaangażowanie Google w dostarczanie narzędzi dla programistów, które odpowiadają złożoności i skali dzisiejszych aplikacji. Nowicjusze powinni jednak być świadomi nieco wyższego progu nauki i konieczności zapoznania się z koncepcjami Google Cloud Platform. Mimo to, przejście jest korzystne dla programistów chcących w pełni wykorzystać możliwości chmury. To dostosowanie do usług chmurowych jest częścią szerszego trendu w rozwoju oprogramowania, podkreślającym znaczenie solidnych, skalowalnych mechanizmów logowania w erze obliczeń chmurowych.
