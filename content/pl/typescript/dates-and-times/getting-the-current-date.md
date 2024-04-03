---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:11:07.745246-07:00
description: "Pobieranie bie\u017C\u0105cej daty w TypeScript, j\u0119zyku opartym\
  \ na JavaScript, pozwala na dost\u0119p i manipulacj\u0119 bie\u017C\u0105cymi informacjami\
  \ o dacie i czasie. Programi\u015Bci\u2026"
lastmod: '2024-03-13T22:44:35.150173-06:00'
model: gpt-4-0125-preview
summary: "Pobieranie bie\u017C\u0105cej daty w TypeScript, j\u0119zyku opartym na\
  \ JavaScript, pozwala na dost\u0119p i manipulacj\u0119 bie\u017C\u0105cymi informacjami\
  \ o dacie i czasie."
title: Pobieranie aktualnej daty
weight: 29
---

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
