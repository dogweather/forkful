---
title:    "TypeScript: Porównywanie dwóch dat"
keywords: ["TypeScript"]
---

{{< edit_this_page >}}

## Dlaczego

Porównywanie dat jest jednym z podstawowych zadań, które musimy wykonać w wielu aplikacjach. Jest to niezbędne w przypadku tworzenia harmonogramów, systemów rezerwacji biletów czy też aplikacji związanych z czasem. W tym artykule dowiesz się, jak porównywać daty w TypeScript i dlaczego jest to ważne dla Twojego kodu.

## Jak to zrobić

Porównywanie dat w TypeScript jest bardzo proste. Musimy jednak pamiętać, że TypeScript to nadzbiór języka JavaScript, więc większość funkcjonalności dotyczącej porównywania dat działa tak samo jak w JavaScript. Poniżej znajdziesz kilka przykładów kodu oraz efektów wywołania.

```TypeScript
const date1: Date = new Date('2020-01-01');
const date2: Date = new Date('2021-01-01');

// Porównanie dat za pomocą operatorów porównania
console.log(date1 < date2); // Wyświetli true
console.log(date2 > date1); // Wyświetli true
console.log(date1 === date2); // Wyświetli false, ponieważ daty są różne

// Sprawdzenie kolejności dat
console.log(date2.getTime() > date1.getTime()); // Wyświetli true, ponieważ daty są w kolejności rosnącej
console.log(date1.getTime() === date2.getTime()); // Wyświetli false, ponieważ daty są różne
```

Jak widać powyżej, możemy porównywać daty za pomocą operatorów porównania oraz poprzez porównywanie wartości funkcji `getTime()`, która zwraca liczbowy czas w milisekundach. Ważne jest, aby używać odpowiednich operatorów, ponieważ daty są obiektami i nie można ich porównywać bezpośrednio.

## Deep Dive

Głębsze zrozumienie porównywania dat może być przydatne, szczególnie w przypadku bardziej skomplikowanych scenariuszy. W TypeScript mamy możliwość używania funkcji `Date()` oraz `getTime()` do uzyskania wartości liczbowych dat, ale istnieje również wiele bibliotek, które oferują bardziej zaawansowane funkcje porównywania dat. Jedną z nich jest moment.js, która jest bardzo popularna wśród programistów JavaScript.

Moment.js jest oparta na języku JavaScript i dostępna jest również dla TypeScript. Daje ona możliwość przetwarzania i manipulowania datami w bardziej intuicyjny sposób. Na przykład, możemy wykorzystać funkcję `isBefore()` do porównania dwóch dat i sprawdzenia, czy jedna jest wcześniejsza od drugiej.

```TypeScript
import moment from 'moment';

const date1:string = '2020-01-01';
const date2:string = '2021-01-01';

const isBefore:boolean = moment(date1).isBefore(date2); // Zwróci true

if(isBefore) {
  console.log('Data 1 jest wcześniejsza niż data 2');
}
```

Możemy również wykorzystać moment.js do bardziej skomplikowanych operacji, takich jak określenie, czy dwa przedziały dat nachodzą na siebie czy też nie. Dzięki tym dodatkowym funkcjom możemy mieć większą kontrolę nad porównywaniem dat i łatwiej radzić sobie z trudniejszymi scenariuszami.

## Zobacz również

- Oficjalna dokumentacja TypeScript (https://www.typescriptlang.org/docs/)
- Porównywanie dat w JavaScript (https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Date)
- Moment.js (https://momentjs.com)