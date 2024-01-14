---
title:    "Javascript: Znajdowanie długości ciągu znaków"
keywords: ["Javascript"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/pl/javascript/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

# Dlaczego?

Dlaczego warto nauczyć się znajdować długość ciągu znaków w Javascript? Jest to podstawowa umiejętność, która jest niezbędna do pracy z tekstem w dowolnej aplikacji. Pozwala ona na wykonywanie różnych operacji, takich jak sprawdzanie warunków, wycinanie fragmentów tekstu czy tworzenie nowych ciągów. W tym artykule pokażemy Ci jak to zrobić krok po kroku.

# Jak To Zrobić?

Aby znaleźć długość ciągu znaków w Javascript, możesz skorzystać z wbudowanej funkcji `length()`. Przykładowy kod można zapisać w następujący sposób:

```Javascript
let str = "To jest przykładowy ciąg znaków.";
console.log(str.length); // Wyświetli 32
```

W powyższym przykładzie deklarujemy zmienną `str` oraz przypisujemy do niej ciąg znaków. Następnie za pomocą funkcji `length()` wyświetlamy długość tego ciągu. Jak widać, ten prosty kod pozwala na znalezienie długości ciągu w Javascript.

Możemy również wykorzystać pętlę `for` do przejrzenia wszystkich znaków w danym ciągu i wyliczenia jego długości. Poniższy przykład pokazuje jak to zrobić:

```Javascript
let str = "Jestem programistą Javascript.";
let length = 0;

for (let i = 0; i < str.length; i++) {
  length++;
}

console.log(length); // Wyświetli 31
```

W tym przypadku deklarujemy zmienną `length`, która będzie zliczać ilość znaków, następnie za pomocą pętli `for` przechodzimy przez wszystkie znaki w ciągu i zwiększamy wartość zmiennej `length` za każdym razem, gdy znajdziemy nowy znak. Po zakończeniu pętli wyświetlamy wartość zmiennej, która w tym przypadku jest równa długości ciągu.

# Deep Dive

Gdy już nauczysz się podstaw znajdowania długości ciągu znaków w Javascript, możesz przejść dalej i poznać kilka ciekawych rzeczy na ten temat. Na przykład, chciałbyś być w stanie obliczyć długość tylko wybranych części ciągu? Możesz to zrobić za pomocą metody `substring()` i funkcji `length()` jak w poniższym przykładzie:

```Javascript
let str = "Javascript jest super!";
let part = str.substring(0, 10); // Wybieramy pierwsze 10 znaków z ciągu
console.log(part.length); // Wyświetli 10 
```

Powyższy kod najpierw wybiera pierwsze 10 znaków z ciągu `str` za pomocą metody `substring()`, a następnie za pomocą funkcji `length()` wylicza ich długość. Możesz eksperymentować i zmieniać wartości w metodzie `substring()` aby wybierać różne części ciągu i sprawdzać ich długość.

# Zobacz też

Jeśli chcesz dowiedzieć się więcej o manipulacji tekstu w Javascript, polecamy przeczytać artykuły w poniższych linkach:

- [W3Schools - String Length](https://www.w3schools.com/jsref/jsref_length_string.asp)
- [Mozilla Developer Network - String](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/String)