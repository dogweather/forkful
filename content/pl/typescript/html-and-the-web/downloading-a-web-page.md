---
title:                "Pobieranie strony internetowej"
aliases:
- /pl/typescript/downloading-a-web-page/
date:                  2024-01-20T17:44:53.198425-07:00
model:                 gpt-4-1106-preview
simple_title:         "Pobieranie strony internetowej"

tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/typescript/downloading-a-web-page.md"
---

{{< edit_this_page >}}

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
