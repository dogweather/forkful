---
title:                "Rejestrowanie zdarzeń"
aliases:
- pl/javascript/logging.md
date:                  2024-01-26T01:07:36.402883-07:00
model:                 gpt-4-1106-preview
simple_title:         "Rejestrowanie zdarzeń"

tag:                  "Good Coding Practices"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/javascript/logging.md"
---

{{< edit_this_page >}}

## Co i dlaczego?
Logowanie, w największym skrócie, przypomina prowadzenie dziennika dla Twojej aplikacji – rejestruje zdarzenia, błędy i inne znaczące akcje, które mają miejsce podczas działania oprogramowania. Programiści robią to nie tylko, aby zrozumieć co się dzieje pod maską w czasie rzeczywistym, ale również by mieć historyczny zapis, który jest kluczowy do debugowania, audytowania i optymalizacji wydajności.

## Jak to zrobić:
W standardzie, JavaScript oferuje prosty sposób na logowanie wiadomości do konsoli:

```javascript
console.log('To zostanie zalogowane w konsoli');

// Wyjście:
// To zostanie zalogowane w konsoli
```

Ale aplikacje w realnym świecie wymagają czegoś więcej niż tylko drukowanie wiadomości do konsoli. Biblioteki takie jak Winston czy Pino mogą być wprowadzone, aby efektywnie zarządzać logami:

```javascript
// Używanie Winston do zaawansowanego logowania
const winston = require('winston');

const logger = winston.createLogger({
  level: 'info',
  format: winston.format.json(),
  transports: [
    new winston.transports.File({ filename: 'combined.log' })
  ],
});

logger.info('Cześć, to jest zdarzenie logowania za pomocą Winston');
// Ten log jest zapisany do 'combined.log' w formacie JSON
```

Przykładowe wyjście `combined.log`:

```json
{"message":"Cześć, to jest zdarzenie logowania za pomocą Winston","level":"info"}
```

## Wnikliwe spojrzenie
Logowanie jest niezbędne od wczesnych dni komputeryzacji; operatorzy systemów przeglądali logi, aby zrozumieć wydajność systemu i diagnozować problemy. Przeskakując do nowoczesnego rozwoju, przeszliśmy od prostych plików logów do strukturyzowanych i przeszukiwalnych systemów zarządzania logami.

Alternatywy dla logowania do konsoli lub pliku w JavaScript obejmują wykorzystanie usług logowania w chmurze, takie jak Loggly, Datadog, czy ELK Stack (Elasticsearch, Logstash, Kibana), które mogą agregować logi z wielu źródeł, oferują narzędzia do wizualizacji i zaawansowaną analitykę.

Podczas implementacji logowania, rozważ następujące kwestie:
- **Poziom szczegółowości**: Włącznie z debugowaniem, informacjami, ostrzeżeniami, błędami i krytycznymi.
- **Wydajność**: Nadmierne logowanie może wpłynąć na wydajność aplikacji.
- **Bezpieczeństwo**: Bądź ostrożny z logowaniem wrażliwych informacji.
- **Format**: Strukturyzowane logi (jak JSON) ułatwiają przeszukiwanie i analizę logów.
- **Polityki retencji**: Stare logi muszą być archiwizowane lub usuwane, aby oszczędzać miejsce.

Praktyczna strategia logowania określa co logować, gdzie to logować i jak długo to zachowywać, balansując pomiędzy informacyjnym wglądem a rozważaniami dotyczącymi wydajności i prywatności.

## Zobacz również
Sprawdź te zasoby, aby pogłębić temat:
- [Repozytorium GitHub Winston](https://github.com/winstonjs/winston): dla dogłębnych zastosowań i niestandardowych transportów.
- [Pino - Bardzo niewielkie obciążenie logger Node.js](https://github.com/pinojs/pino): lekkie rozwiązanie do logowania.
- [MDN Web Docs: Konsola](https://developer.mozilla.org/en-US/docs/Web/API/Console): podstawowe informacje o logowaniu w przeglądarce.
- [Elastic ELK Stack](https://www.elastic.co/what-is/elk-stack): potężna trójca do zarządzania logami.
- [Logowanie aplikacji 12 Factor](https://12factor.net/logs): najlepsze praktyki logowania aplikacji.
