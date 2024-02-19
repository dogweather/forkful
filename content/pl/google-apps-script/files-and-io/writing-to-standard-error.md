---
aliases:
- /pl/google-apps-script/writing-to-standard-error/
changelog:
- 2024-02-01, gpt-4-0125-preview, translated from English
date: 2024-02-01 22:08:57.402354-07:00
description: "Pisanie do standardowego b\u0142\u0119du (stderr) w j\u0119zykach programowania\
  \ dotyczy kierowania komunikat\xF3w o b\u0142\u0119dach i diagnostyki do oddzielnego\
  \ strumienia,\u2026"
lastmod: 2024-02-18 23:08:49.170562
model: gpt-4-0125-preview
summary: "Pisanie do standardowego b\u0142\u0119du (stderr) w j\u0119zykach programowania\
  \ dotyczy kierowania komunikat\xF3w o b\u0142\u0119dach i diagnostyki do oddzielnego\
  \ strumienia,\u2026"
title: "Pisanie do standardowego b\u0142\u0119du"
---

{{< edit_this_page >}}

## Co i dlaczego?

Pisanie do standardowego błędu (stderr) w językach programowania dotyczy kierowania komunikatów o błędach i diagnostyki do oddzielnego strumienia, odseparowanego od standardowego wyjścia (stdout). Programiści robią to, aby oddzielić normalne wyjście programu od komunikatów o błędach, co ułatwia debugowanie i analizę logów.

## Jak to zrobić:

Google Apps Script, będąc językiem skryptowym do lekkiego rozwoju aplikacji na platformie Google Apps, nie zapewnia bezpośredniej wbudowanej funkcji takiej jak `console.error()` do pisania do stderr, jak można znaleźć w Node.js czy Pythonie. Można jednak symulować to zachowanie, używając usług logowania Google Apps Script lub niestandardowego obsługiwania błędów do zarządzania i segregowania wyjść błędów.

### Przykład: Użycie `Logger` dla komunikatów o błędach

```javascript
function logError() {
  try {
    // Symulacja błędu
    const result = 1 / 0;
    if(!isFinite(result)) throw new Error("Próba dzielenia przez zero");
  } catch (e) {
    // Zapisz komunikat o błędzie do Logów
    Logger.log('Błąd: ' + e.message);
  }
}
```

Kiedy uruchomisz `logError()`, spowoduje to zapisanie komunikatu o błędzie do logów Google Apps Script, które można wyświetlić w opcji `Widok > Logi`. To nie jest dokładnie stderr, ale służy podobnemu celowi oddzielania logów błędów od standardowych wyjść.

### Zaawansowane logowanie diagnostyczne

Do bardziej zaawansowanego debugowania i logowania błędów możesz użyć Stackdriver Logging, obecnie znanego jako Google Cloud's Operations Suite.

```javascript
function advancedErrorLogging() {
  try {
    // Celowe spowodowanie błędu
    const obj = null;
    const result = obj.someProperty;
  } catch (e) {
    console.error('Napotkano błąd: ', e.toString());
  }
}
```

Spowoduje to skierowanie komunikatu o błędzie do Stackdriver Logging, gdzie jest zarządzany jako log na poziomie błędu. Zauważ, że integracja Stackdriver/Google Cloud’s Operations Suite oferuje bardziej szczegółowe i przeszukiwalne rozwiązanie logowania w porównaniu z `Logger`.

## Głębsze spojrzenie

Brak dedykowanego strumienia `stderr` w Google Apps Script odzwierciedla jego naturę i pochodzenie jako języka skryptowego opartego na chmurze, gdzie tradycyjne wyjścia konsoli lub terminala (takie jak stdout i stderr) są mniej istotne. Historycznie, Google Apps Script został zaprojektowany do wzmacniania funkcjonalności Google Apps za pomocą prostych skryptów, skupiając się na łatwości użycia kosztem kompleksowych funkcji dostępnych w bardziej złożonych środowiskach programistycznych.

Mimo to, ewolucja Google Apps Script w kierunku bardziej zaawansowanego rozwoju aplikacji skłoniła programistów do przyjmowania kreatywnych podejść do obsługi błędów i logowania, wykorzystując dostępne usługi takie jak Logger i integrując się z Google Cloud’s Operations Suite. Te metody, choć nie są bezpośrednimi implementacjami stderr, oferują solidne alternatywy dla zarządzania błędami i logowania diagnostycznego w środowisku zorientowanym na chmurę.

Krytycznie, choć te metody spełniają swoje zadanie w ekosystemie Google Apps Script, podkreślają one ograniczenia platformy w porównaniu z tradycyjnymi środowiskami programistycznymi. Dla programistów wymagających szczegółowych i hierarchicznych strategii obsługi błędów, integracja z zewnętrznymi usługami logowania lub przyjęcie Google Cloud Functions, które oferują bardziej konwencjonalne obsługi stderr i stdout, może być preferowana.
