---
title:    "TypeScript: Wycinanie podciągów"
keywords: ["TypeScript"]
---

{{< edit_this_page >}}

## Dlaczego

Jedną z ważnych umiejętności każdego programisty jest umiejętność manipulowania ciągami znaków, zwłaszcza jeśli chcemy pracować z tekstem. Jednym z często wykorzystywanych zadań jest wyodrębnianie podciągów, czyli fragmentów tekstu, które są częścią większego ciągu znaków. W tym artykule dowiesz się, dlaczego warto nauczyć się tej umiejętności i jak ją wykonować w języku TypeScript.

## Jak to zrobić

W języku TypeScript możemy wykorzystać metodę `substring()` z obiektu `String`, aby wyodrębnić fragment ciągu znaków. Metoda ta przyjmuje dwa parametry - indeks początkowy i indeks końcowy podciągu. Przykładowy kod wyglądałby następująco:

```TypeScript
let str = "To jest przykładowy tekst.";
let subStr = str.substring(3, 10);
console.log(subStr);
```

W powyższym przykładzie wyodrębnimy fragment tekstu od trzeciego znaku (indeks 3) do dziesiątego znaku (indeks 9), otrzymując wynik "jest pr".

## Deep Dive

Podczas wyodrębniania podciągów warto wiedzieć, że indeksy znaków zaczynają się od zera, więc pierwszy znak w ciągu ma indeks 0. Możemy także wykorzystać tę metodę do wyodrębniania fragmentów tekstu na podstawie wzoru. Na przykład, jeśli znamy indeks początkowy, ale nie końcowy podciągu, możemy użyć wartości `str.length` jako indeksu końcowego, aby wyodrębnić resztę tekstu od danego znaku.

Warto także pamiętać, że metoda `substring()` zwraca wyodrębniony podciąg jako nowy obiekt `String`, więc możemy go przypisać do zmiennej i dalej z niego korzystać.

## Zobacz także

Jeśli chcesz dowiedzieć się więcej o manipulowaniu ciągami znaków w języku TypeScript, możesz przeczytać ten artykuł: [Working with Strings in TypeScript](https://blog.bitsrc.io/working-with-strings-in-typescript-4edbd4d5d68a) lub sprawdzić dokumentację na oficjalnej stronie języka: [String.prototype.substring()](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/String/substring).