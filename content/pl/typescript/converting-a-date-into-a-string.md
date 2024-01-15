---
title:                "Konwertowanie daty na ciąg znaków"
html_title:           "TypeScript: Konwertowanie daty na ciąg znaków"
simple_title:         "Konwertowanie daty na ciąg znaków"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/typescript/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## Dlaczego

Prawdopodobnie każdy programista ma do czynienia z konwersją daty na łańcuch znaków w swojej codziennej pracy. Jest to powszechna operacja, która jest niezbędna do wyświetlania daty użytkownikom lub zapisywania jej w bazie danych. W tym artykule dowiesz się, jak w prosty sposób przekonwertować datę na łańcuch znaków przy użyciu TypeScript, języka programowania, który jest obecnie bardzo popularny w świecie front-endu i back-endu.

## Jak to zrobić

Konwersja daty na łańcuch znaków w TypeScript jest niezwykle prosta i wymaga zaledwie kilku linijek kodu. Aby to zrobić, musisz użyć metody `toLocaleString()` z obiektu `Date` i podać odpowiednie parametry, takie jak format daty oraz lokalizacja. Poniżej znajduje się przykładowy kod, który przekonwertuje obecną datę i godzinę na łańcuch znaków w formacie „dd.MM.yyyy HH:mm”:

```TypeScript
const currentDate = new Date();
const convertedDate = currentDate.toLocaleString('pl-PL', { 
    day: '2-digit',
    month: '2-digit',
    year: 'numeric',
    hour: '2-digit',
    minute: '2-digit'
});

console.log(convertedDate);
// wyświetli: "18.07.2021 12:30"
```

Możesz również dostosować format wyjściowy, używając innych parametrów, takich jak `weekday`, `era` czy `timeZone`. Dokładną listę wszystkich dostępnych parametrów można znaleźć w dokumentacji TypeScript.

## Deep Dive

Podczas konwersji daty na łańcuch znaków warto zwrócić uwagę na to, jakie lokalizacje są używane, aby format daty był zgodny z oczekiwaniami użytkowników. Warto również wiedzieć, że w niektórych przypadkach daty mogą być wyświetlane w różnych formatach w zależności od ustawień systemowych użytkownika. Dlatego zawsze warto przetestować działanie swojego kodu w różnych lokalizacjach, aby mieć pewność, że wszystko działa prawidłowo.

Jednym z przydatnych narzędzi, które może pomóc w konwersji daty na łańcuch znaków, jest biblioteka moment.js, która oferuje więcej funkcjonalności i możliwości formatowania daty. Jest to szczególnie przydatne, jeśli musisz wyświetlać daty w różnych strefach czasowych lub w bardziej skomplikowany sposób.

Oczywiście, istnieje także możliwość tworzenia własnych funkcji lub używania innych bibliotek, które mogą pomóc w konwersji daty na łańcuch znaków. Ważne jest jednak, aby zawsze dbać o poprawność formatowania i uwzględniać możliwe różnice w wyświetlaniu dat w różnych lokalizacjach.

## Zobacz również

- [Dokumentacja TypeScript dla metody toLocaleString()](https://developer.mozilla.org/pl/docs/Web/JavaScript/Reference/Global_Objects/Date/toLocaleString)
- [Biblioteka moment.js](https://momentjs.com/)