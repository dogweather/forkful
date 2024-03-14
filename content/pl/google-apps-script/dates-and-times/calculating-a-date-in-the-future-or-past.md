---
changelog:
- 2024-02-01, gpt-4-0125-preview, translated from English
date: 2024-02-01 21:49:02.011325-07:00
description: "Obliczanie daty w przysz\u0142o\u015Bci lub przesz\u0142o\u015Bci polega\
  \ na manipulowaniu obiektami daty, aby znale\u017A\u0107 daty poza obecn\u0105 dat\u0105\
  \ lub przed ni\u0105, odpowiednio.\u2026"
lastmod: '2024-03-13T22:44:34.918132-06:00'
model: gpt-4-0125-preview
summary: "Obliczanie daty w przysz\u0142o\u015Bci lub przesz\u0142o\u015Bci polega\
  \ na manipulowaniu obiektami daty, aby znale\u017A\u0107 daty poza obecn\u0105 dat\u0105\
  \ lub przed ni\u0105, odpowiednio.\u2026"
title: "Obliczanie daty w przysz\u0142o\u015Bci lub przesz\u0142o\u015Bci"
---

{{< edit_this_page >}}

## Co i dlaczego?

Obliczanie daty w przyszłości lub przeszłości polega na manipulowaniu obiektami daty, aby znaleźć daty poza obecną datą lub przed nią, odpowiednio. Programiści robią to do zadań, począwszy od ustawiania przypomnień i dat wygaśnięcia, po analizowanie trendów danych opartych na czasie.

## Jak to zrobić:

W Google Apps Script, który opiera się na JavaScript, można manipulować datami za pomocą obiektu `Date`. Oto jak obliczyć daty w przyszłości i w przeszłości:

### Obliczanie daty w przyszłości

Aby obliczyć datę w przyszłości, tworzysz obiekt daty dla bieżącej daty, a następnie dodajesz do niego pożądaną liczbę dni (lub innych jednostek czasu).

```javascript
// Bieżąca data
var today = new Date();

// Oblicz datę 10 dni w przyszłości
var futureDate = new Date(today);
futureDate.setDate(today.getDate() + 10);

Logger.log("Data w przyszłości: " + futureDate.toDateString());
```

### Obliczanie daty w przeszłości

Podobnie, aby znaleźć datę w przeszłości, odejmujesz liczbę dni od bieżącej daty.

```javascript
// Bieżąca data
var today = new Date();

// Oblicz datę 10 dni w przeszłości
var pastDate = new Date(today);
pastDate.setDate(today.getDate() - 10);

Logger.log("Data w przeszłości: " + pastDate.toDateString());
```

### Przykładowe wyjście

To generuje coś w rodzaju poniższego (zakładając, że dziś jest 15 kwietnia 2023):

```
Data w przyszłości: Wt kwie 25 2023
Data w przeszłości: Śr kwie 05 2023
```

Pamiętaj, że obiekt Date w JavaScript (i tym samym w Google Apps Script) automatycznie dostosowuje miesiące i lata, gdy dodajesz lub odejmujesz dni.

## Pogłębiona analiza

Manipulowanie datami za pomocą obiektu `Date` wywodzi się z wczesnych implementacji JavaScript. Z czasem to podejście generalnie pozostało spójne, dostarczając programistom prostego sposobu na zarządzanie datami bez potrzeby używania zewnętrznych bibliotek. Jednakże, do bardziej skomplikowanych operacji, takich jak dostosowania strefy czasowej, czy przy pracy z obszernymi danymi bazującymi na datach, biblioteki takie jak `Moment.js` lub nowocześniejszy `Luxon` mogą oferować większą funkcjonalność i łatwiejsze obsługiwanie.

W Google Apps Script, konkretnie, pomimo bezpośredniej dostępności i prostoty obiektu `Date`, ważne jest, aby być świadomym, jak obliczenia dat mogą wpływać na wydajność skryptu i czas wykonania, szczególnie w wyzwalaczach opartych na czasie lub obszernych manipulacjach arkuszy kalkulacyjnych. Ponadto, chociaż Google Apps Script oferuje wbudowane metody do obsługi dat w jego ekosystemie (takie jak w Google Sheets czy Calendar), integracja zewnętrznych bibliotek lub wykorzystanie Zaawansowanych Usług Google czasami może zapewnić bardziej rozbudowane rozwiązania dla skomplikowanych scenariuszy.

Zatem, chociaż natywna metodologia obiektu JavaScript `Date` jest zazwyczaj wystarczająca dla prostych obliczeń, eksploracja zewnętrznych bibliotek lub usług może zwiększyć funkcjonalność dla bardziej złożonych wymagań.
