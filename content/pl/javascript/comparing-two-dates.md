---
title:                "Porównywanie dwóch dat"
html_title:           "C++: Porównywanie dwóch dat"
simple_title:         "Porównywanie dwóch dat"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/javascript/comparing-two-dates.md"
---

{{< edit_this_page >}}

## Co i Dlaczego?

Porównywanie dwóch dat jest, jak sama nazwa wskazuje, mechanizmem znalezienia różnicy między tymi dwoma punktami w czasie. Programiści robią to głównie, aby kontrolować sekwencję zdarzeń, wyliczać różne statystyki i monitorować postęp.

## Jak to zrobić:

Porównywanie dwóch dat w JavaScript jest proste. Oto przykładowy kod:

```Javascript 
let data1 = new Date('2022-01-01');
let data2 = new Date('2022-02-01');

if(data1 < data2) {
  console.log("data1 jest wcześniejsza");
} else if(data1 > data2) {
  console.log("data1 jest późniejsza");
} else {
  console.log("daty są takie same");
}
```
Jeśli uruchomisz ten kod, zobaczysz, że wydrukuje "data1 jest wcześniejsza", ponieważ 1 stycznia 2022 jest wcześniejszy od 1 lutego 2022.

## Deep Dive:

Porównanie dat w JavaScript jest standardem od początku istnienia języka, ale jest wiele różnych metod, które można zastosować do praktycznej realizacji tego zadania. Oprócz powyższego przykładu, inne metody obejmują konwersję dat na liczbę milisekund od konkretnej daty w przeszłości (znanej jako Unix Epoch), a następnie porównanie tych liczb.

Jest wiele bibliotek, takich jak moment.js, które oferują bardziej rozbudowane metody porównywania dat, które mogą być lepszym rozwiązaniem dla bardziej skomplikowanych problemów.

Szczegół implementacyjny: wewnętrznie, JavaScript przechowuje daty jako liczby reprezentujące milisekundy od północy 1 stycznia 1970 r. UTC. To jest przyczyną, dlaczego bezpośrednie porównanie jest możliwe.

## Zobacz także:

Jeśli chcesz dowiedzieć się więcej na temat pracy z datami w JavaScript, oto kilka przydatnych linków:

- Dokumentacja Mozilla: [Date (JavaScript)](https://developer.mozilla.org/pl/docs/Web/JavaScript/Reference/Global_Objects/Date)
- Poradnik moment.js: [Manipulacja i formatowanie dat](https://momentjs.com/)
- Artykuł o porównywaniu dat: [Porównywanie dat w JavaScript](https://dmitripavlutin.com/compare-dates-javascript/)
- Artykuł Stack Overflow: [Jak porównać dwie daty bez uwzględniania czasu](https://stackoverflow.com/questions/2698725/comparing-date-part-only-without-comparing-time-in-javascript)