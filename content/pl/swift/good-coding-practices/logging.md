---
date: 2024-01-26 01:08:16.980724-07:00
description: "Jak to zrobi\u0107: W Swift mo\u017Cesz zapisywa\u0107 logi na konsoli\
  \ za pomoc\u0105 instrukcji print lub bardziej elastycznego interfejsu API `os.log`,\
  \ kt\xF3ry \u0142\u0105czy si\u0119 z\u2026"
lastmod: '2024-03-13T22:44:35.761334-06:00'
model: gpt-4-1106-preview
summary: "W Swift mo\u017Cesz zapisywa\u0107 logi na konsoli za pomoc\u0105 instrukcji\
  \ print lub bardziej elastycznego interfejsu API `os.log`, kt\xF3ry \u0142\u0105\
  czy si\u0119 z Zintegrowanym Systemem Logowania na platformach Apple."
title: "Rejestrowanie zdarze\u0144"
weight: 17
---

## Jak to zrobić:
W Swift możesz zapisywać logi na konsoli za pomocą instrukcji print lub bardziej elastycznego interfejsu API `os.log`, który łączy się z Zintegrowanym Systemem Logowania na platformach Apple.

```Swift
import os.log

let logger = OSLog(subsystem: "com.yourapp.domain", category: "network")

func fetchData() {
    // Prosta instrukcja print
    print("Rozpoczęto pobieranie")
    
    // Logowanie zdarzenia na poziomie informacji za pomocą os.log
    os_log(.info, log: logger, "Pobieranie danych z API.")
    
    do {
        let data = try performNetworkRequest()
        // Logowanie zdarzenia na poziomie debugowania
        os_log(.debug, log: logger, "Otrzymano dane: %@", data.description)
    } catch {
        // Logowanie zdarzenia na poziomie błędu
        os_log(.error, log: logger, "Nie udało się pobrać danych: %@", error.localizedDescription)
    }
}

func performNetworkRequest() throws -> Data {
    // Symulacja żądania sieciowego
    return Data()
}
```

Przykładowe wyjście na konsoli może wyglądać tak:

```
Rozpoczęto pobieranie
Pobieranie danych z API.
Otrzymano dane: Kilka bajtów danych...
```

W przypadku błędów może to być:

```
Nie udało się pobrać danych: Wydaje się, że połączenie internetowe jest offline.
```

## Szczegółowa analiza
Logowanie w Swift nabrało nowej mocy i efektywności dzięki Zintegrowanemu Systemowi Logowania wprowadzonemu w iOS 10 i macOS Sierra. W przeciwieństwie do instrukcji `print`, która kieruje bezpośrednio na konsolę, system ten jest oparty na aktywnościach i pozwala filtrować komunikaty logów na podstawie ich ważności oraz tego, czy są to buildy debugowania czy wydania.

Historyczny kontekst kreśli ewolucję logowania w iOS i macOS od podstawowych instrukcji print do kompleksowych narzędzi, które integrują się z aplikacją Instruments i Konsolą, dostarczając zaawansowanych sposobów na analizę logów.

Istnieje szereg alternatyw dla logowania w Swift, takich jak biblioteki firm trzecich, na przykład CocoaLumberjack, która oferuje warstwę makr nad Zintegrowanym Systemem Logowania. Zapewnia ona rozszerzoną kontrolę nad formatowaniem logów, zarządzaniem plikami oraz opcjami wydajności.

Na koniec, szczegóły implementacji; OSLog jest zaprojektowany nie tylko po to, by być efektywny, ale również z myślą o ochronie prywatności, z możliwością zamazywania prywatnych danych podczas logowania. Kategoryzuje logi na poziomy awarii, błędu, informacji i debugowania, każdy oferujący różną szczegółowość do rozwiązywania problemów.

## Zobacz również
- [Dokumentacja Zintegrowanego Systemu Logowania od Apple](https://developer.apple.com/documentation/os/logging)
- [Poradnik logowania Ray'a Wenderlicha](https://www.raywenderlich.com/605079-logging-in-swift-oslog)
- [Repozytorium CocoaLumberjack na GitHub](https://github.com/CocoaLumberjack/CocoaLumberjack)
