---
date: 2024-01-20 17:44:53.198425-07:00
description: "How to: (Jak to zrobi\u0107:) Do pobierania stron internetowych w TypeScript\
  \ u\u017Cyjemy biblioteki `axios`. Oto przyk\u0142ad."
lastmod: '2024-04-05T21:53:36.578627-06:00'
model: gpt-4-1106-preview
summary: "(Jak to zrobi\u0107:) Do pobierania stron internetowych w TypeScript u\u017C\
  yjemy biblioteki `axios`."
title: Pobieranie strony internetowej
weight: 42
---

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
