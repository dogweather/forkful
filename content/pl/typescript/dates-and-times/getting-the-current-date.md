---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:11:07.745246-07:00
description: "Jak to zrobi\u0107: W TypeScript mo\u017Cesz u\u017Cy\u0107 obiektu\
  \ `Date`, aby uzyska\u0107 bie\u017C\u0105c\u0105 dat\u0119 i czas. Oto jak mo\u017C\
  esz to zrobi\u0107."
lastmod: '2024-03-13T22:44:35.150173-06:00'
model: gpt-4-0125-preview
summary: "W TypeScript mo\u017Cesz u\u017Cy\u0107 obiektu `Date`, aby uzyska\u0107\
  \ bie\u017C\u0105c\u0105 dat\u0119 i czas."
title: Pobieranie aktualnej daty
weight: 29
---

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
