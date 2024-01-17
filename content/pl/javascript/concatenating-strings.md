---
title:                "Łączenie ciągów znaków"
html_title:           "Javascript: Łączenie ciągów znaków"
simple_title:         "Łączenie ciągów znaków"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/javascript/concatenating-strings.md"
---

{{< edit_this_page >}}

Co i dlaczego?

Konkatenacja ciągów to po prostu połączenie dwóch lub więcej ciągów znaków w jedną dłuższą linię. Programiści wykorzystują tę technikę, aby łączyć różne elementy w celu wyświetlania tekstu lub budowania bardziej skomplikowanych napisów.

Jak to zrobić:

👨‍💻 W JavaScript, możemy wykorzystać operator + do konkatenacji ciągów znaków:

```javascript
let pierwszyCiąg = "Witaj";
let drugiCiąg = "świecie!";
let wynik = pierwszyCiąg + " " + drugiCiąg;
console.log(wynik); // Wyświetla "Witaj świecie!"
```

👨‍💻 Możemy również użyć metody concat():

```javascript
let pierwszyCiąg = "Hello";
let drugiCiąg = "world!";
let wynik = pierwszyCiąg.concat(" ", drugiCiąg);
console.log(wynik); // Wyświetla "Hello world!"
```

Najważniejsze jest to, aby pamiętać, aby wszystkie elementy były ciągami znaków. W przeciwnym razie mogą pojawić się problemy z działaniem kodu.

Pogłębione informacje:

🔍 Konkatenacja ciągów była możliwa już w języku programowania C, a następnie stała się standardem w innych językach, w tym w JavaScript. Alternatywą dla konkatenacji jest użycie szablonów literałów, które mogą być bardziej czytelne i łatwiejsze do zrozumienia.

⚙️ W rzeczywistości, kiedy używamy operatora + do konkatenacji, wewnętrznie używany jest operator +=. W związku z tym, kiedy używamy konkatenacji wielokrotnie, może to mieć wpływ na wydajność kodu.

Zobacz również:

🔗 [Dokumentacja JavaScript na temat konkatenacji ciągów](https://developer.mozilla.org/pl/docs/Web/JavaScript/Reference/Global_Objects/String/concat)

🔗 [Inne sposoby na łączenie ciągów w JavaScript](https://www.freecodecamp.org/news/three-ways-to-concatenate-strings-in-javascript-8850286b697c/)