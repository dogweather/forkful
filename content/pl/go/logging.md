---
title:                "Rejestrowanie zdarzeń"
date:                  2024-01-26T01:07:19.362073-07:00
model:                 gpt-4-1106-preview
simple_title:         "Rejestrowanie zdarzeń"

tag:                  "Good Coding Practices"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/go/logging.md"
---

{{< edit_this_page >}}

## Co i dlaczego?
Logowanie polega na rejestrowaniu wydarzeń, stanów i przepływu danych w aplikacji. Programiści robią to w celu diagnozowania błędów, monitorowania wydajności i śledzenia stanu operacyjnego aplikacji - co praktycznie czyni je oprogramowaniowym odpowiednikiem czarnej skrzynki w samolotach.

## Jak to zrobić:
W Go, logowanie może być obsługiwane na wiele sposobów, począwszy od standardowej biblioteki `log`, po biblioteki firm trzecich, takie jak `logrus` i `zap`. Oto prosty przykład użycia wbudowanego pakietu `log`:

```Go
package main

import (
	"log"
	"os"
)

func main() {
	// Tworzenie pliku logów
	logFile, err := os.OpenFile("app.log", os.O_CREATE|os.O_APPEND|os.O_WRONLY, 0666)
	if err != nil {
		log.Fatal(err)
	}
	defer logFile.Close()

	// Ustawienie wyjścia logu do pliku
	log.SetOutput(logFile)

	// Logowanie pewnych zdarzeń
	log.Println("Uruchamianie aplikacji...")
	// ... tutaj logika aplikacji ...
	log.Println("Aplikacja zakończyła działanie pomyślnie.")
}
```

Jeśli uruchomisz ten kod, nie zobaczysz żadnego wyjścia w terminalu, ponieważ wszystko trafia do pliku `app.log`. Oto jak wyglądają wpisy, które znajdziesz w tym pliku logów:

```
2023/01/02 15:04:05 Uruchamianie aplikacji...
2023/01/02 15:05:01 Aplikacja zakończyła działanie pomyślnie.
```

## Szczegółowa analiza
Logowanie w programowaniu sięga czasów pierwszych komputerów, kiedy to inżynierowie dosłownie znajdowali owady (ćmy, żeby być dokładnym) rozgniecione w sprzęcie, które następnie rejestrowali! Przeskakując do dzisiaj, logowanie stało się wyrafinowanym sposobem na zrozumienie, co dzieje się wewnątrz złożonych systemów.

Chociaż pakiet `log` w Go jest dość prosty, może być wystarczający dla podstawowych aplikacji. Jednak w kontekście nowoczesnych rozproszonych systemów, lub gdy potrzebujesz bardziej wyrafinowanej kontroli nad swoim wyjściem logów (jak różne poziomy powagi), możesz chcieć rozważyć bardziej rozbudowane rozwiązania.

Biblioteki logowania firm trzecich, takie jak `logrus` i `zap`, oferują strukturalne logowanie, co oznacza, że możesz logować skomplikowane typy danych, takie jak JSON, co ułatwia interpretację logów, szczególnie w połączeniu z systemami zarządzania logami takimi jak ELK Stack czy Splunk.

Przy rozważaniu implementacji strategii logowania, istotne jest również myślenie o implikacjach wydajnościowych. Biblioteki logowania o wysokiej wydajności są zoptymalizowane pod kątem zmniejszania wpływu na przepustowość i opóźnienie aplikacji. Na przykład, `zap` chwali się swoim ekspresowym, niskim alokowaniem projektu, co może być kluczowe dla systemów czasu rzeczywistego.

Oprócz różnych bibliotek, również logowanie formatów i standardów są warte uwagi. Strukturalne formaty logowania, takie jak JSON, mogą być niezwykle potężne, gdy są używane w połączeniu z systemami przetwarzania logów. Z drugiej strony, logi w postaci zwykłego tekstu są czytelne dla człowieka, ale trudniejsze do programowego przetworzenia.

## Zobacz również
Aby zagłębić się w możliwości logowania w Go, te zasoby mogą być przydatne:

- Blog Go na temat logowania: https://blog.golang.org/logging
- `logrus`, strukturalny logger dla Go: https://github.com/sirupsen/logrus
- `zap`, szybki, strukturalny, uwarstwiony logger: https://github.com/uber-go/zap
- ELK Stack (Elasticsearch, Logstash, Kibana) do analizy logów: https://www.elastic.co/what-is/elk-stack
- Porównanie bibliotek logowania Go: https://www.loggly.com/blog/benchmarking-5-popular-golang-logging-libraries/
