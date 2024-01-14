---
title:    "TypeScript: Pobieranie bieżącej daty"
keywords: ["TypeScript"]
---

{{< edit_this_page >}}

## Dlaczego
W dzisiejszych czasach programowanie jest nieodłączną częścią naszego życia. Od aplikacji mobilnych po strony internetowe, jesteśmy otoczeni kodem. Jednym z popularnych języków programowania jest TypeScript, który pozwala nam pisać wydajny i skalowalny kod. Ale czy wiesz, że możesz również użyć TypeScript do pobierania bieżącej daty? W tym artykule dowiesz się, w jaki sposób możesz wykorzystać TypeScript do pobrania aktualnej daty i czasu.

## Jak to zrobić
Aby uzyskać aktualną datę w TypeScript, musimy użyć funkcji wbudowanej o nazwie `Date()`. Ta funkcja zwraca obiekt daty, który może być przetworzony w różne sposoby. Na przykład, aby wyświetlić aktualną datę, możemy użyć metody `toDateString()` w następujący sposób:
```TypeScript
let currentDate = new Date();
console.log(currentDate.toDateString());
```
Wyjście tego kodu będzie wyglądać mniej więcej tak:
```
Sat Aug 28 2021
```
Możemy również wyświetlić godzinę i minutę, używając metody `toLocaleTimeString()`:
```TypeScript
let currentTime = new Date();
console.log(currentTime.toLocaleTimeString());
```
To spowoduje wyświetlenie aktualnego czasu w formacie 24-godzinnym:
```
16:20:50
```
Istnieje wiele innych metod i opcji, które możemy wykorzystać do przetwarzania i wyświetlania aktualnej daty w TypeScript. Zachęcam do eksperymentowania z nimi i do szukania informacji w dokumentacji języka.

## Głębsze zagęszczenie
Teraz, gdy wiesz, jak pobierać aktualną datę w TypeScript, możesz się zastanawiać, jak dokładnie działa ta funkcja `Date()`. Otóż, jest ona oparta na czasie uniwersalnym (UTC) i zwraca obiekt daty, który jest ustawiony na bieżącą datę i czas według strefy czasowej twojego komputera. Możesz także wykorzystać inne metody, takie jak `getMonth()` czy `getFullYear()` do uzyskania szczegółowych informacji o dacie. Warto również pamiętać, że istnieją biblioteki takie jak Moment.js, które ułatwiają pracę z datami i czasem w TypeScript.

## Zobacz również
- [Dokumentacja TypeScript](https://www.typescriptlang.org/docs/)
- [Moment.js](https://momentjs.com/)