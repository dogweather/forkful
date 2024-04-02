---
date: 2024-01-20 17:44:53.198425-07:00
description: "Pobieranie strony internetowej to proces \u015Bci\u0105gania jej danych.\
  \ Programi\u015Bci robi\u0105 to, aby przetworzy\u0107, przeszuka\u0107 lub wyszuka\u0107\
  \ konkretn\u0105 tre\u015B\u0107."
lastmod: '2024-03-13T22:44:35.137740-06:00'
model: gpt-4-1106-preview
summary: "Pobieranie strony internetowej to proces \u015Bci\u0105gania jej danych.\
  \ Programi\u015Bci robi\u0105 to, aby przetworzy\u0107, przeszuka\u0107 lub wyszuka\u0107\
  \ konkretn\u0105 tre\u015B\u0107."
title: Pobieranie strony internetowej
weight: 42
---

## What & Why? (Co i Dlaczego?)
Pobieranie strony internetowej to proces ściągania jej danych. Programiści robią to, aby przetworzyć, przeszukać lub wyszukać konkretną treść.

## How to: (Jak to zrobić:)
Do pobierania stron internetowych w TypeScript użyjemy biblioteki `axios`. Oto przykład:

```typescript
import axios from 'axios';

async function downloadWebPage(url: string): Promise<string> {
  try {
    const response = await axios.get(url);
    return response.data; // zawartość strony jako string
  } catch (error) {
    throw new Error(`Nie można pobrać strony: ${error}`);
  }
}

// Użycie funkcji:
downloadWebPage('https://www.example.com')
  .then(data => console.log(data))
  .catch(error => console.error(error));
```

Jeśli wszystko pójdzie dobrze, zobaczysz HTML strony w konsoli.

## Deep Dive (Dogłębna analiza)
Pierwsze narzędzia do pobierania stron powstały już w latach 90., na przykład `wget`. Obecnie, oprócz `axios`, popularne są `fetch` (wbudowane w nowoczesne przeglądarki i środowisko Node.js) czy `puppeteer` do zaawansowanych scenariuszy z przeglądarką.

Alternatywy mają różne moce: `fetch` ma mniejszy rozmiar i jest natywnie wspierane, `puppeteer` umożliwia interakcje z JavaScript na stronie. Dlatego ważne jest wybranie odpowiedniego narzędzia do zadania. Detale implementacyjne mogą obejmować obsługę ciasteczek, nagłówków HTTP czy obsługę sesji.

## See Also (Zobacz także)
- Dokumentacja `axios`: https://www.npmjs.com/package/axios
- Porównanie `fetch` i `axios`: https://www.smashingmagazine.com/2020/06/rest-api-axios-fetch-javascript/
- Strona projektu `puppeteer`: https://pptr.dev/
