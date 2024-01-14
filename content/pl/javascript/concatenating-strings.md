---
title:    "Javascript: Łączenie ciągów tekstowych"
keywords: ["Javascript"]
---

{{< edit_this_page >}}

## Dlaczego

Witajcie Javascriptowcy! Jeśli jesteście nowicjuszami w programowaniu, pewnie zastanawiacie się, po co nam w ogóle łączyć ciągi tekstu? Aby odpowiedzieć na to pytanie, musimy najpierw zrozumieć, czym są ciągi tekstowe i dlaczego są one ważne w naszych programach.

Ciągi tekstowe są po prostu sekwencjami znaków, które składają się na nasze słowa, zdania i teksty. W programowaniu często musimy manipulować tymi ciągami tekstu, czy to dla celów prezentacyjnych, czy dla przetwarzania danych. Jednym z najważniejszych sposobów manipulowania ciągami tekstu jest po prostu łączenie ich razem - czyli tzw. konkatenacja. 

Teraz możemy rozpocząć naszą przygodę z łączeniem ciągów tekstowych w Javascript! 

## Jak to zrobić

Przede wszystkim musimy zdefiniować zmienne, które będą przechowywać nasze ciągi tekstowe. Możemy to zrobić przy użyciu słowa kluczowego `var` lub `let`, a następnie przypisać do nich nasze ciągi tekstowe, używając cudzysłowów. Na przykład:

```Javascript
var imie = "Kasia";
var nazwisko = "Kowalska";
```

Teraz możemy połączyć te zmienne za pomocą operatora `+`, tak jakbyśmy chcieli dodać dwie liczby. Pamiętajmy tylko, aby dodać również spacje, jeśli chcemy zachować odpowiedni format naszego tekstu. Oto przykładowy kod:

```Javascript
var pelneImie = imie + " " + nazwisko;
console.log(pelneImie);
```

Powyższy kod wyświetli w konsoli: `Kasia Kowalska`. 

Ale co jeśli chcemy połączyć więcej niż dwa ciągi tekstowe? W takim przypadku możemy użyć metody `.concat()`, która pozwala nałączać więcej niż dwie zmienne. Przykładowo:

```Javascript 
var zawolanie = "Witaj, ";
var komunikat = zawolanie.concat(imie, "! Miło cię widzieć!");
console.log(komunikat);
```

Wyjściem tego kodu będzie: `Witaj, Kasia! Miło cię widzieć!`.

Możemy również wykorzystać konkatenację w celu łączenia liter i ciągów znaków w jedno wyrażenie. Na przykład:

```Javascript
var inicjaly = imie[0] + "." + nazwisko[0] + ".";
console.log(inicjaly);
```

Ten kod wyświetli na ekranie: `K.K.`

## Wnikliwy przegląd

W przypadku tego prostego przykładu może wydawać się, że konkatenacja nie jest zbyt skomplikowana. Ale im bardziej zaawansowane aplikacje tworzymy, tym bardziej złożone operacje możemy wykonać na naszych ciągach tekstowych. 

Na przykład możemy użyć metody `.slice()` do wycinania fragmentów ciągów tekstowych i łączenia ich w nowe wyrażenia. Możemy również użyć operatora `+=` aby w prosty sposób dodać nowe ciągi tekstowe do naszych istniejących zmiennych. Możliwości jest wiele, a tylko dzięki łączeniu ciągów tekstowych jesteśmy w stanie tworzyć coraz to ciekawsze funkcjonalności w naszych programach.

## Zobacz też

Teraz, gdy znasz podstawy konkatenacji ciągów tekstowych w Javascript, możesz spróbować swoich sił w tworzeniu własnych przykładowych kodów i eksperymentować z różnymi metodami i operatorami. Poniżej znajdziesz