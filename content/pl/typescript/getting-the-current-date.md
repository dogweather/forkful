---
title:                "Pobieranie aktualnej daty"
html_title:           "TypeScript: Pobieranie aktualnej daty"
simple_title:         "Pobieranie aktualnej daty"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/typescript/getting-the-current-date.md"
---

{{< edit_this_page >}}

## Co i dlaczego?
Pobieranie aktualnej daty jest ważnym aspektem dla wielu programistów. Jest to proces, który pozwala programowi uzyskać bieżące informacje o dacie i czasie. Jest to szczególnie przydatne przy tworzeniu aplikacji, które muszą śledzić aktualne wydarzenia lub operować na danych z różnych dni.

## Jak to zrobić:
### Przykłady kodu w języku TypeScript
Poniżej znajdziesz kilka przykładów kodu w języku TypeScript, które pozwolą Ci pobierać aktualną datę w różnych formatach.

#### Pobranie daty w formacie DD/MM/YYYY (03/11/2021)
```TypeScript
const currentDate = new Date();
const day = currentDate.getDate();
const month = currentDate.getMonth() + 1;
const year = currentDate.getFullYear();
console.log(`${day}/${month}/${year}`);
```
#### Pobranie daty w formacie DD-MM-YYYY (03-11-2021)
```TypeScript
const currentDate = new Date();
const options = { day: '2-digit', month: '2-digit', year: 'numeric' };
console.log(currentDate.toLocaleDateString('en-US', options));
```
#### Pobranie godziny w formacie HH:MM:SS (15:30:45)
```TypeScript
const currentDate = new Date();
const hour = currentDate.getHours();
const minutes = currentDate.getMinutes();
const seconds = currentDate.getSeconds();
console.log(`${hour}:${minutes}:${seconds}`);
```

## Deep Dive:
### Kontekst historyczny:
Pobieranie aktualnej daty jest istotne, ponieważ pozwala programom na prawidłowe wykonywanie operacji, które wymagają informacji o bieżącym czasie. Wcześniej konieczne było ręczne obliczanie daty, co nie tylko zajmowało więcej czasu, ale również było bardziej podatne na błędy.

### Alternatywy:
Podczas pobierania aktualnej daty w TypeScript można również użyć bibliotek, takich jak Moment.js lub Day.js, które udostępniają wiele funkcji i formatów daty.

### Szczegóły implementacji:
Do pobrania aktualnej daty w TypeScript używa się obiektu `Date`. Jest to globalny obiekt w języku JavaScript, który przechowuje informacje o bieżącym czasie. Można również wykorzystać metodę `toLocaleDateString()` do określenia pożądanego formatu daty.

## Zobacz także:
- [Dokumentacja TypeScript: Obiekt Date](https://www.typescriptlang.org/docs/handbook/intro-to-js-ts.html#date)
- [Moment.js: Biblioteka do obsługi dat w JavaScript](https://momentjs.com/)
- [Day.js: Prosta biblioteka do manipulowania datami w JavaScript](https://day.js.org/en/)