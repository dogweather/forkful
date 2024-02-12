---
title:                "Pobieranie aktualnej daty"
aliases:
- pl/typescript/getting-the-current-date.md
date:                  2024-02-03T19:11:07.745246-07:00
model:                 gpt-4-0125-preview
simple_title:         "Pobieranie aktualnej daty"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/typescript/getting-the-current-date.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Co i Dlaczego?
Pobieranie bieżącej daty w TypeScript, języku opartym na JavaScript, pozwala na dostęp i manipulację bieżącymi informacjami o dacie i czasie. Programiści często potrzebują tej funkcjonalności do tworzenia znaczników czasu, planowania i innych funkcji związanych z czasem w swoich aplikacjach.

## Jak to zrobić:
W TypeScript możesz użyć obiektu `Date`, aby uzyskać bieżącą datę i czas. Oto jak możesz to zrobić:

```typescript
const currentDate = new Date();
console.log(currentDate);
```

Przykładowe wyjście:
```
2023-04-12T07:20:50.52Z
```

Ten fragment kodu tworzy nowy obiekt `Date` zawierający bieżącą datę i czas, który następnie jest wyświetlany w konsoli. Możesz również sformatować datę za pomocą funkcji toLocaleDateString(), aby uzyskać bardziej czytelne formaty:

```typescript
const currentDate = new Date();
console.log(currentDate.toLocaleDateString());
```

Przykładowe wyjście:
```
12.04.2023
```

### Użycie biblioteki date-fns
Do bardziej zaawansowanej manipulacji i formatowania daty popularny jest wybór biblioteki `date-fns`. Najpierw zainstaluj ją za pomocą npm:

```bash
npm install date-fns
```

Następnie możesz jej użyć, aby sformatować bieżącą datę:

```typescript
import { format } from 'date-fns';

const currentDate = new Date();
console.log(format(currentDate, 'yyyy-MM-dd'));
```

Przykładowe wyjście:
```
2023-04-12
```

Ten przykład z użyciem `date-fns` formatuje bieżącą datę jako łańcuch znaków w formacie "RRRR-MM-DD". Biblioteka oferuje mnóstwo funkcji do manipulacji datą, czyniąc ją wszechstronnym narzędziem dla każdego programisty TypeScript pracującego z datami.
