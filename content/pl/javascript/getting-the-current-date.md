---
title:                "Pobieranie aktualnej daty"
html_title:           "Arduino: Pobieranie aktualnej daty"
simple_title:         "Pobieranie aktualnej daty"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/javascript/getting-the-current-date.md"
---

{{< edit_this_page >}}

## Co to jest i po co?

Pobieranie aktualnej daty to metoda wykorzystania wbudowanych funkcji JavaScript do określenia bieżącej daty i czasu. Robimy to, aby śledzić zmiany, rejestrować zdarzenia lub kontekst i kiedykolwiek musimy wykonać czynność w określonym czasie.

## Jak to zrobić:

Pozyskujmy aktualną datę za pomocą obiektu `Date` JavaScript. 

```Javascript
let teraz = new Date();
console.log(teraz);
```

Wykonywanie tego kodu będzie zwracać coś w stylu:

``` 
2022-06-28T19:15:28.484Z
```

## Szerzej na temat:

(1) Historia:
Początkowo JavaScript nie zawierał funkcji do obsługi daty i czasu. Dodano je do języka w 1997 roku wraz z wprowadzeniem ECMAScript.

(2) Alternatywy:
Istnieją biblioteki, takie jak moment.js, które oferują zaawansowane funkcje do manipulacji i formatowania dat.

(3) Implementacja:
Obiekt `Date` JavaScript korzysta z systemu czasu Unix, pomiaru czasu jako liczby milisekund od północy 1 stycznia 1970 roku.

```Javascript
let teraz = Date.now();
console.log(teraz);
```

Ten fragment kodu zwróci coś takiego:

```
1640837769632
```

Oznacza to, że upłynęło 1640837769632 milisekundy od początku ery Unix.

## Zobacz też:

1. Dokumentacja MDN na temat obiektu Date: https://developer.mozilla.org/pl/docs/Web/JavaScript/Referencje/Obiekty/Date

2. Dokumentacja moment.js: https://momentjs.com/ 

Pamiętaj, inteligentne korzystanie z daty i czasu może sprawić, że Twój kod będzie bardziej efektywny i użyteczny!