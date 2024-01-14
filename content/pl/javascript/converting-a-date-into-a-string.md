---
title:    "Javascript: Konwersja daty na ciąg znaków"
keywords: ["Javascript"]
---

{{< edit_this_page >}}

## Dlaczego konwertować datę na ciąg znaków?

Programowanie w języku JavaScript może być nieco skomplikowane, zwłaszcza jeśli jesteś początkującym programistą. Jednym z zadań, które możesz napotkać, jest konwersja daty na ciąg znaków. Ale po co w ogóle to robić? Dlaczego ta funkcja jest ważna w programowaniu? Czy jest to tylko czysta formalność, czy też jest to coś, co może pomóc w ulepszeniu Twojego kodu? W tym artykule wyjaśnimy dlaczego warto uczyć się konwertować datę na ciąg znaków w języku JavaScript.

## Jak to zrobić?

Aby przekonwertować datę na ciąg znaków w języku JavaScript, musimy użyć metody `toString()`. Oto przykładowy kod:

```Javascript
const date = new Date();
const stringDate = date.toString();
console.log(stringDate);
```

Ten kod wyświetli datę w formacie standardowym ustawionym w Twojej przeglądarce, na przykład "Sat May 29 2021 18:20:45 GMT+0200 (Central European Summer Time)". Ale co, jeśli chcesz mieć pełną kontrolę nad tym, jak data jest wyświetlana? W tym przypadku musimy użyć metody `toLocaleString()`. Przyjrzyjmy się przykładowemu kodowi:

```Javascript
const date = new Date();
const stringDate = date.toLocaleString("pl-PL", {
  weekday: "long",
  year: "numeric",
  month: "long",
  day: "2-digit",
});
console.log(stringDate);
```

W tym przypadku użyliśmy metody `toLocaleString()` z argumentami "pl-PL", czyli językiem i miejscowością, oraz z dodatkowym obiektem, w którym określiliśmy wymagane parametry wyświetlania daty. Dzięki temu możemy uzyskać bardziej spersonalizowany format daty, np. "sobota, 29 maja 2021".

## Głębsze zanurzenie

Konwertowanie daty na ciąg znaków może wydawać się prostym zadaniem, ale jest kilka rzeczy, które warto wiedzieć, aby zapewnić odpowiednie wyświetlanie daty. Na przykład, jeśli nie określisz jasno języka i miejscowości w metodzie `toLocaleString()`, data może zostać wyświetlona w formacie angielskim lub innym, a to może być niepożądane. Dlatego ważne jest, aby zawsze dokładnie ustawić te parametry.

Dodatkowo, istnieje wiele innych metod, które można użyć do konwertowania daty na różne formaty w języku JavaScript, takie jak `getFullYear()`, `getMonth()`, `getDate()` i wiele innych. Warto zapoznać się z nimi, aby lepiej zrozumieć sposób działania dat w tym języku.

## Zobacz także

- [Dokumentacja MDN na temat konwersji daty na ciąg znaków w języku JavaScript](https://developer.mozilla.org/pl/docs/Web/JavaScript/Referencje/Obiekty/Date/toString)
- [Funkcje daty w języku JavaScript](https://www.w3schools.com/js/js_date_methods.asp)
- [Przydatne wskazówki i triki dotyczące dat w języku JavaScript](https://www.digitalocean.com/community/tutorials/working-with-dates-in-javascript)