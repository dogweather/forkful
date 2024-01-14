---
title:                "Javascript: Wyszukiwanie i zamiana tekstu"
simple_title:         "Wyszukiwanie i zamiana tekstu"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/javascript/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## Dlaczego

Programowanie to dziedzina, która wymaga nie tylko wiedzy i umiejętności, ale także nieustannego poszukiwania i przetwarzania danych. W przypadku różnych projektów często zachodzi potrzeba dokonywania zmian w dużych zbiorach tekstu. W takich sytuacjach bardzo przydatne okazują się narzędzia do wyszukiwania i zamiany tekstu. W artykule tym przyjrzymy się temu zagadnieniu oraz omówimy jakie narzędzia są dostępne w języku Javascript.

## Jak To Zrobić

Do wykonania operacji wyszukiwania i zamiany tekstu w języku Javascript możemy użyć kilku różnych metod. Jedną z najpopularniejszych jest metoda `replace()`, która pozwala na podmianę wyrażenia regularnego na inny ciąg znaków. Przykład użycia tej metody wygląda następująco:

```Javascript
let tekst = "Jestem programistą, zajmuję się tworzeniem aplikacji webowych.";
let nowyTekst = tekst.replace(/programistą/g, "developerem");
```

W powyższym przykładzie wykorzystaliśmy wyrażenie regularne `/programistą/g`, które wyszukuje wszystkie wystąpienia słowa "programistą". Następnie używając metody `replace()` zamieniliśmy to słowo na "developerem". Wynik tego działania zapisaliśmy do zmiennej `nowyTekst`, która będzie zawierała tekst "Jestem developerem, zajmuję się tworzeniem aplikacji webowych.".

Inną przydatną metodą jest `split()`, która pozwala na rozdzielenie tekstu na tablicę znaków na podstawie określonego separatora. Przykład użycia tej metody wygląda następująco:

```Javascript
let tekst = "Jestem programistą, zajmuję się tworzeniem aplikacji webowych.";
let tekstTablica = tekst.split(",");
```

W powyższym przykładzie używając metody `split(",")` podzieliliśmy tekst na dwie części na podstawie przecinka, dzięki czemu w wyniku otrzymaliśmy tablicę z dwoma elementami: "Jestem programistą" oraz "zajmuję się tworzeniem aplikacji webowych.".

## Wnikliwa Analiza

Większość metod służących do wyszukiwania i zamiany tekstu w języku Javascript wykorzystuje wyrażenia regularne. Są to specjalne ciągi znaków, które pozwalają na precyzyjne określenie wzorów wyszukiwania. Podstawowymi elementami wyrażeń regularnych są tak zwane "metaznaki", czyli znaki o specjalnym znaczeniu. Przykładowe metaznaki to:

- `.` - oznacza dowolny pojedynczy znak
- `*` - oznacza dowolną ilość wystąpień danego znaku lub zestawu znaków
- `+` - oznacza jeden lub więcej wystąpień danego znaku lub zestawu znaków
- `?` - oznacza zero lub jeden wystąpienie danego znaku lub zestawu znaków

Ponadto wyrażenia regularne w języku Javascript umożliwiają również wykorzystanie tzw. grup, czyli fragmentów wyrażenia, które można wyodrębnić za pomocą nawiasów. Przykład wykorzystania grupy wygląda następująco:

```Javascript
let tekst = "Witaj, nazywam się John";
let imie = tekst.replace(/Witaj, nazywam się ([A-Z][a-z]+)/, "$1");
```

W powyższym przykładzie wykorzystaliśmy grupę `([A-Z][a-z]+)`, która odpowiada jednemu wyrazowi z