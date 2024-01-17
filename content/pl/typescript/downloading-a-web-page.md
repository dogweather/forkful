---
title:                "Pobieranie strony internetowej"
html_title:           "TypeScript: Pobieranie strony internetowej"
simple_title:         "Pobieranie strony internetowej"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/typescript/downloading-a-web-page.md"
---

{{< edit_this_page >}}

Cześć programiści!

## O co chodzi & Dlaczego?

Ściąganie strony internetowej to proces pobrania zawartości strony internetowej na nasz komputer. Programiści często wykonują ten krok, aby mieć dostęp do informacji lub aby przetworzyć je w programowaniu.

## Jak to zrobić:

```TypeScript
const request = require('request');

request.get('https://www.example.com/', (error, response, body) => {
  console.log(body);
});
```

Output:
`<html> ... </html>`

## Wchodzimy w szczegóły:

Ściąganie stron internetowych jest jednym z głównych procesów wykorzystywanych w web scrapingu (pobieranie danych z internetu). Innym sposobem na zrobienie tego jest wykorzystanie narzędzi takich jak Selenium, ale jest to bardziej złożony proces i wymaga użycia przeglądarki internetowej.

## Zobacz także:

Jeśli chcesz się dowiedzieć więcej o web scrapingu, możesz przeczytać artykuł "Web Scraping: Co to jest i dlaczego jest ważne?" na stronie [https://www.edureka.co/blog/web-scraping/](https://www.edureka.co/blog/web-scraping/)