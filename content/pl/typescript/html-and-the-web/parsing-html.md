---
date: 2024-01-20 15:34:39.122013-07:00
description: "How to: (Jak to zrobi\u0107:) Parsing HTML nie zawsze by\u0142 tak prosty\
  \ jak teraz. W przesz\u0142o\u015Bci u\u017Cywano skomplikowanych regularnych wyra\u017C\
  e\u0144, kt\xF3re niezbyt dobrze\u2026"
lastmod: '2024-04-05T22:50:49.440197-06:00'
model: unknown
summary: "(Jak to zrobi\u0107:) Parsing HTML nie zawsze by\u0142 tak prosty jak teraz."
title: Przetwarzanie HTML
weight: 43
---

## How to: (Jak to zrobić:)
```TypeScript
import { JSDOM } from 'jsdom';

// przykładowy HTML do sparsowania
const exampleHTML = `
    <div id="greetings">
        <p>Hello, World!</p>
        <p>Witaj, Świecie!</p>
    </div>
`;

// tworzenie DOM z HTML
const dom = new JSDOM(exampleHTML);

// uzyskanie dostępu do elementów
const greetings = dom.window.document.querySelector('#greetings');

// wyświetlenie zawartości
console.log(greetings.textContent);
// Wyjście: 
// Hello, World!
// Witaj, Świecie!
```

## Deep Dive (Dogłębna Analiza)
Parsing HTML nie zawsze był tak prosty jak teraz. W przeszłości używano skomplikowanych regularnych wyrażeń, które niezbyt dobrze radziły sobie z niestandardowym kodem HTML. Błogi czas przed DOM i jQuery!

Alternatywy? Oczywiście! Istnieją inne biblioteki, na przykład `cheerio` lub `parse5`, które również pozwalają na analizę HTML w Node.js.

Jeśli chodzi o implementację, najważniejsza jest wydajność i dokładność działania. Biblioteka `jsdom`, którą użyliśmy w przykładzie, naśladuje DOM w środowisku Node.js, co ułatwia zadanie, ale czasami może być trochę za wolna. W przypadkach, gdzie liczy się szybkość, może lepszym wyborem będzie `cheerio`, które poświęca precyzję na rzecz wydajności.

## See Also (Zobacz również)
- MDN Web Docs na temat DOM: https://developer.mozilla.org/en-US/docs/Web/API/Document_Object_Model
- Oficjalna dokumentacja `jsdom`: https://github.com/jsdom/jsdom
- Dokumentacja `cheerio`: https://cheerio.js.org/
- Dokumentacja `parse5`: https://github.com/inikulin/parse5

Wykorzystanie tych zasobów pozwoli Ci zagłębić się w świat parsowania HTML i zrozumieć, jak różne narzędzia są dostosowane do różnych potrzeb.
