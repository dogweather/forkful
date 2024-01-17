---
title:                "Łączenie ciągów znaków"
html_title:           "TypeScript: Łączenie ciągów znaków"
simple_title:         "Łączenie ciągów znaków"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/typescript/concatenating-strings.md"
---

{{< edit_this_page >}}

## Co i dlaczego?
Łączenie ciągów znaków jest podstawowym narzędziem w programowaniu. Polega ono na łączeniu różnych fragmentów tekstu w jedną dłuższą ciągłą linię. Programiści stosują tę technikę, aby tworzyć bardziej czytelny i praktyczny kod.

## Jak to zrobić:
Przykład prostego łączenia dwóch ciągów znaków w TypeScript wygląda następująco: 
```TypeScript
let imie: string = "Kasia";
let nazwisko: string = "Kowalska";
let pelne_imie = imie + " " + nazwisko;
console.log(pelne_imie);
```

W wyniku otrzymamy:
```TypeScript
Kasia Kowalska
```

Możemy również łączyć więcej niż dwa ciągi znaków. Na przykład:
```TypeScript
let opcja1: string = "opcja1";
let opcja2: string = "opcja2";
let opcja3: string = "opcja3";
let wybor = "Możesz wybrać: " + opcja1 + ", " + opcja2 + " lub " + opcja3;
console.log(wybor);
```

Otrzymamy:
```TypeScript
Możesz wybrać: opcja1, opcja2 lub opcja3
```

## Głębsze spojrzenie:
Łączenie ciągów znaków jest powszechnie używaną techniką w wielu językach programowania. Wcześniej, w językach takich jak C czy Java, zwykle korzystano ze specjalnych funkcji do łączenia ciągów. W TypeScript mamy na to prostsze rozwiązanie - można po prostu użyć operatora "+" do łączenia ciągów. Alternatywą do łączenia ciągów jest również szablonowanie, co jest szczególnie przydatne przy łączeniu większej liczby fragmentów tekstu.

## Zobacz też:
Jeśli chcesz poznać więcej na temat łączenia ciągów w TypeScript, warto zajrzeć na stronę dokumentacji Microsoft: https://www.typescriptlang.org/docs/handbook/basic-types.html#string. Możesz też zapoznać się z innymi funkcjonalnościami języka, które mogą być przydatne w Twoim kodzie.