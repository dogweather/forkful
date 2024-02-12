---
title:                "Przetwarzanie HTML"
aliases:
- pl/typescript/parsing-html.md
date:                  2024-01-20T15:34:39.122013-07:00
simple_title:         "Przetwarzanie HTML"

tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/typescript/parsing-html.md"
---

{{< edit_this_page >}}

## What & Why? (Co i Dlaczego?)
Parsing HTML to proces analizy kodu HTML, by zrozumieć jego strukturę i wyodrębnić z niego dane. Programiści robią to, żeby manipulować tym kodem lub pobierać informacje z dokumentów HTML, które można wykorzystać w różnych aplikacjach.

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
