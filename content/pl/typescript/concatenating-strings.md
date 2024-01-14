---
title:    "TypeScript: Konkatenacja łańcuchów znaków"
keywords: ["TypeScript"]
---

{{< edit_this_page >}}

## Dlaczego

Chociaż może się wydawać to proste, łączenie ciągów znaków jest bardzo ważną częścią pracy programisty w TypeScript. Dzięki temu możemy tworzyć dynamiczne i spójne wyświetlanie danych w naszych aplikacjach. Jest to jedna z podstawowych umiejętności, które musi posiadać każdy programista.

## Jak to zrobić

Aby dołączyć dwa ciągi znaków w TypeScript, możemy skorzystać z operatora plus `+` lub funkcji `concat()`. Oba te rozwiązania działają w podobny sposób. Przykładowy kod wyglądałby następująco:

```TypeScript
let message1 = 'Cześć';
let message2 = 'Świecie';

console.log(message1 + ' ' + message2);
console.log(message1.concat(' ', message2));

// Output:
// Cześć Świecie
// Cześć Świecie
```

Jak widać, wynikiem obu operacji jest to samo - połączone ciągi znaków. W pierwszym przypadku wykorzystujemy operator plus, a w drugim funkcję `concat()`, która jako argumenty przyjmuje kolejne ciągi znaków. Warto zauważyć, że w obu przypadkach dodajemy także spację, aby oddzielić słowa w wynikowym ciągu.

Jeśli chcielibyśmy dołączyć więcej niż dwa ciągi, możemy po prostu dodać kolejne argumenty do funkcji `concat()`, lub użyć operatora plus i kilka razy wykorzystać zmienną z wynikiem.

## Deep Dive

Najważniejszym aspektem przy łączeniu ciągów jest pamiętanie o typach danych. W TypeScript każda zmienna musi mieć określony typ, dlatego też należy uważać na to, jakie zmienne łączymy. Jeśli próbujemy dołączyć do siebie zmienne różnych typów, TypeScript może zwrócić błąd, co może skutkować niepoprawnym działaniem naszego programu.

Kolejną ważną kwestią jest wydajność. Operator plus jest zazwyczaj szybszy od funkcji `concat()`, dlatego też warto napisać kod w ten sposób, żeby unikać wywoływania tych funkcji w pętli lub wykorzystywać zmienną z wynikiem do łączenia kolejnych ciągów.

## Zobacz też

Jeśli chcesz dowiedzieć się więcej o manipulacji ciągami znaków w TypeScript, polecamy poniższe artykuły:

* [MDN - String concatenation](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Operators/Addition_assignment)
* [TypeScript Deep Dive - Strings](https://basarat.gitbooks.io/typescript/content/docs/types/literal-types.html)