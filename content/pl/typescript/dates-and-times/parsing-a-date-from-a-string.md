---
title:                "Analiza składniowa daty z łańcucha znaków"
aliases: - /pl/typescript/parsing-a-date-from-a-string.md
date:                  2024-02-03T19:16:00.926835-07:00
model:                 gpt-4-0125-preview
simple_title:         "Analiza składniowa daty z łańcucha znaków"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/typescript/parsing-a-date-from-a-string.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Co i dlaczego?
Parsowanie daty z ciągu znaków polega na konwertowaniu tekstowych reprezentacji dat i czasów na format, który może być przetwarzany i analizowany przez program. Jest to częste zadanie w programowaniu, ponieważ pozwala na obsługę danych wejściowych od użytkownika, przechowywanie danych z czasem oraz interakcje z API, co prowadzi do tworzenia bardziej funkcjonalnych i przyjaznych dla użytkownika aplikacji.

## Jak to zrobić:
TypeScript, będąc nadzbiorem JavaScript, polega na obiekcie Date do parsowania dat z ciągów znaków. Jednak praca z datami w JS/TS może stać się rozwlekła lub nieprecyzyjna z powodu dziwactw obiektu Date. Oto podstawowy przykład, po którym następuje podejście z użyciem popularnej biblioteki, `date-fns`, dla bardziej solidnych rozwiązań.

### Korzystanie z obiektu Date JavaScript
```typescript
// Podstawowe parsowanie przy użyciu konstruktora Date
const dateFromString = new Date("2023-04-21T15:00:00Z");
console.log(dateFromString.toString()); 
// Wyjście dla GMT: "Fri Apr 21 2023 15:00:00 GMT+0000 (Coordinated Universal Time)"
```

Ta metoda działa dla ciągów w formacie ISO i niektórych innych formatów dat, ale może dawać niespójne wyniki dla niejednoznacznych formatów w różnych przeglądarkach i lokalizacjach.

### Korzystanie z date-fns
Biblioteka `date-fns` zapewnia proste i spójne obsługiwanie dat. Jest to biblioteka modułowa, co pozwala na dołączenie tylko tych części, które są potrzebne, redukując rozmiar pakietu.

Pierwsze, zainstaluj `date-fns`: 

```sh
npm install date-fns
```

Następnie, użyj jej do parsowania ciągu daty:

```typescript
import { parseISO, format } from 'date-fns';

// Parsowanie ciągu w formacie ISO
const dateString = "2023-04-21T15:00:00Z";
const parsedDate = parseISO(dateString);

// Formatowanie daty (np. na formę czytelną dla człowieka)
console.log(format(parsedDate, "PPPpp")); 
// Wyjście: "21 kwietnia 2023 o 15:00" (wynik może się różnić w zależności od lokalizacji)
```

`date-fns` obsługuje szeroką gamę formatów i lokalizacji, co czyni go solidnym wyborem dla aplikacji wymagających precyzyjnego parsowania i formatowania dat w różnych regionach użytkowników.
