---
title:                "Analiza składniowa HTML"
html_title:           "Gleam: Analiza składniowa HTML"
simple_title:         "Analiza składniowa HTML"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/typescript/parsing-html.md"
---

{{< edit_this_page >}}

## Co i dlaczego?

Przetwarzanie HTML to proces, dzięki któremu tekst zawierający kod HTML może być analizowany i interpretowany przez komputer. Programiści go używają, aby manipulować, edytować, i interaktywować z danymi na stronie internetowej.

## Jak to zrobić:

```TypeScript
import { parse } from 'fast-html-parser';

let htmlStr = '<div class="powitanie">Cześć, Programisto!</div>';

let root = parse(htmlStr);

console.log(root.querySelector('.powitanie').rawText);
```

Po uruchomieniu powyższego kodu, otrzymasz na wyjściu następujący tekst:

```
Cześć, Programisto!
```

## Deep Dive :

(1) Historycznie, przetwarzanie HTML było dosyć trudne i niewygodne, ale nowoczesne biblioteki, takie jak `fast-html-parser` w TypeScript, uczyniły ten proces dużo łatwiejszym.

(2) Istnieje wiele alternatyw dla `fast-html-parser`, takich jak `jsdom` czy `cheerio`, ale każda z nich ma swoje plusy i minusy. Na przykład, `jsdom` jest dosyć potężny, ale może okazać się zbyt skomplikowany do prostych zadań. Znalezienie odpowiedniego narzędzia zależy od konkretnego przypadku.

(3) To, jak działa przetwarzanie HTML, to dosyć złożony temat. Ale mówiąc najprościej, biblioteka tak jak `fast-html-parser` przegląda cały tekst HTML od początku do końca, identyfikując tagi HTML i zasoby, które one zawierają, a następnie tworzy z nich strukturę danych, którą możemy łatwo przefiltrować i manipulować.

## Zobacz także:

- Dokumentacja `fast-html-parser`: [https://www.npmjs.com/package/fast-html-parser](https://www.npmjs.com/package/fast-html-parser)
- Więcej o `jsdom`: [https://github.com/jsdom/jsdom](https://github.com/jsdom/jsdom)
- Więcej o `cheerio`: [https://cheerio.js.org/](https://cheerio.js.org/)