---
title:                "Analiza składniowa HTML"
aliases:
- /pl/javascript/parsing-html.md
date:                  2024-01-28T03:00:48.718434-07:00
model:                 gpt-4-0125-preview
simple_title:         "Analiza składniowa HTML"

tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/javascript/parsing-html.md"
changelog:
  - 2024-01-28, dogweather, reviewed
  - 2024-01-28, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Co i Dlaczego?
Parsowanie HTML oznacza ekstrakcję danych z dokumentów HTML. Programiści robią to, aby oddziaływać na zawartość sieciową lub ją modyfikować, automatyzować wydobywanie danych lub w celu web scrapingu.

## Jak to zrobić:
Parsujmy HTML za pomocą API `DOMParser` w JavaScript.

```Javascript
const parser = new DOMParser();
const htmlString = `<p>Witaj, świecie!</p>`;
const doc = parser.parseFromString(htmlString, 'text/html');
console.log(doc.body.textContent); // Wynik: Witaj, świecie!
```

Teraz spróbujmy wyłuskać coś bardziej specyficznego, jak element z klasą:

```Javascript
const htmlString = `<div><p class="greeting">Cześć, znowu!</p></div>`;
const doc = parser.parseFromString(htmlString, 'text/html');
const greeting = doc.querySelector('.greeting').textContent;
console.log(greeting); // Wynik: Cześć, znowu!
```

## Dogłębna analiza
Parsowanie HTML istnieje tak długo, jak sieć. Początkowo było to zadanie przeglądarek—parsowały one HTML, aby wyświetlić strony internetowe. Z czasem programiści chcieli mieć dostęp do tego procesu, co doprowadziło do powstania API takich jak `DOMParser`.

Alternatywy? Jasne. Mamy biblioteki takie jak `jQuery` i narzędzia jak `BeautifulSoup` dla Pythona. Ale natywny `DOMParser` JavaScriptu jest szybki i wbudowany, nie potrzeba dodatkowych bibliotek.

Jeśli chodzi o implementację, kiedy parsujesz HTML za pomocą `DOMParser`, tworzony jest obiekt `Document`. Możesz myśleć o nim jako o hierarchicznym modelu twojego HTML. Gdy go masz, możesz nim nawigować i manipulować tak samo jak z normalnym DOM strony internetowej.

Oto rzecz—parsowanie może napotkać problemy na źle sformułowanym HTML. Przeglądarki są wyrozumiałe, ale `DOMParser` już niekoniecznie. Dlatego dla skomplikowanych zadań lub niechlujnego HTML, biblioteki stron trzecich mogą zrobić lepszą robotę przy sprzątaniu.

## Zobacz również
- Dokumentacja MDN na temat API `DOMParser`: [MDN DOMParser](https://developer.mozilla.org/en-US/docs/Web/API/DOMParser)
- Możliwości parsowania w jQuery: [jQuery.parseHTML()](https://api.jquery.com/jquery.parsehtml/)
- Cheerio, szybka, elastyczna i oszczędna implementacja podstawowego jQuery dla serwera: [Cheerio.js](https://cheerio.js.org/)
- Dla parsowania poza JS: biblioteka BeautifulSoup dla Pythona: [Beautiful Soup](https://www.crummy.com/software/BeautifulSoup/)
