---
title:    "Arduino: Usuwanie znaków pasujących do wzoru"
keywords: ["Arduino"]
---

{{< edit_this_page >}}

## Dlaczego

Często w programowaniu, szczególnie ze złożonymi danymi, ​​musimy usuwać znaki, które pasują do pewnego wzoru. Może być to potrzebne do filtrowania błędnych danych lub transformowania danych do pożądanego formatu. W tym artykule dowiesz się, jak łatwo usuwać znaki pasujące do wzoru za pomocą Arduino.

## Jak to zrobić

Do usuwania znaków pasujących do wzoru możemy użyć funkcji `replace()` w Arduino. Spójrzmy na prosty przykład, w którym chcemy usunąć z tekstu wszystkie znaki niebędące liczbami.

```Arduino
String tekst = "jakiś tekst 1234 5678";
tekst.replace("tekst", ""); // usuwa słowo "tekst"
tekst.replace(/[^\d]/g, ""); // usuwa wszystkie znaki niebędące liczbami
Serial.println(tekst); // wyświetli "1234 5678"
```

W powyższym przykładzie, wykorzystujemy funkcję `replace()` do usunięcia słowa "tekst" z naszego tekstu, a następnie wykorzystujemy wyrażenie regularne `/[^\d]/g`, które oznacza "każdy znak niebędący cyfrą", aby usunąć wszystkie znaki niebędące liczbami.

## Pogłębione zagadnienia

Wszystkie argumenty użyte w przykładzie wcześniej, są opcjonalne. Jeśli chcemy wyciąć znaki pasujące do wzoru, możemy przekazać pusty łańcuch do funkcji `replace()`, jak w przypadku słowa "tekst". Jeśli chcemy tylko usunąć znaki pasujące do wzoru, musimy przekazać drugi argument, który określa, przez co mają być one zastąpione. W naszym przypadku, przekazaliśmy pusty łańcuch. Jeśli chcesz bardziej zaawansowanego przetwarzania tekstu, warto zapoznać się z wyrażeniami regularnymi i funkcjami zawartymi w bibliotece `String` w Arduino.

## Zobacz także

Jeśli jesteś zainteresowany innymi podstawowymi manipulacjami tekstu w Arduino, warto przeczytać o podstawowych funkcjach takich jak `substring()` czy `concat()`. Możesz także spróbować zaimplementować algorytmy przeszukiwania tekstu, takie jak algorytm Knutha-Morrisa-Pratta. Poniżej znajdują się przydatne linki do tych zagadnień:

- [Dokumentacja funkcji String w Arduino](https://www.arduino.cc/reference/en/language/variables/data-types/string/functions/replace/)
- [Wyrażenia regularne w Arduino](https://www.arduino.cc/reference/en/language/functions/communication/serial/println/)
- [Alfabetyczna lista funkcji String w Arduino](https://www.arduino.cc/reference/en/language/variables/data-types/string/)
- [Tutorial o algorytmie Knutha-Morrisa-Pratta w języku C](https://www.geeksforgeeks.org/kmp-algorithm-for-pattern-searching/)