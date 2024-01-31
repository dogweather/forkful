---
title:                "Korzystanie z tablic asocjacyjnych"
date:                  2024-01-30T19:10:05.922025-07:00
model:                 gpt-4-0125-preview
simple_title:         "Korzystanie z tablic asocjacyjnych"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Data Structures"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/arduino/using-associative-arrays.md"
changelog:
  - 2024-01-30, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Co i dlaczego?
W świecie Arduino, tablice asocjacyjne pozwalają na parowanie kluczy z wartościami, mniej więcej jak dobieranie skarpetek w pary. Są wyborem numer jeden, kiedy trzeba przechowywać i pobierać dane, używając opisowych nazw, co sprawia, że kod jest czystszy i znacznie bardziej zrozumiały.

## Jak to zrobić:
Arduino, mówiąc ściśle, nie posiada wbudowanego wsparcia dla tablic asocjacyjnych, jakie znajdziesz w językach wyższego poziomu. Ale, nie bój się. Możemy sprytnie używać struktur i tablic, aby naśladować tę funkcjonalność. Oto prosty przykład, jak stworzyć podstawową „tablicę asocjacyjną” do przechowywania i dostępu do temperatur w różnych miastach.

Najpierw zdefiniuj strukturę, która będzie przechowywać miasto (klucz) i jego temperaturę (wartość):

```cpp
struct CityTemperature {
  String city;
  float temperature;
};
```

Następnie, zainicjuj tablicę obiektów `CityTemperature`:

```cpp
CityTemperature temperatures[] = {
  {"New York", 19.5},
  {"Los Angeles", 22.0},
  {"Chicago", 17.0}
};
```

Oto jak można uzyskać dostęp i wyświetlić temperaturę określonego miasta:

```cpp
void setup() {
  Serial.begin(9600);
  for(int i = 0; i < 3; i++) {
    if(temperatures[i].city == "Los Angeles") {
      Serial.print("Temperatura w Los Angeles wynosi: ");
      Serial.println(temperatures[i].temperature);
    }
  }
}

void loop() {
  // Nic tutaj na razie.
}
```

Uruchomienie tego kodu dałoby wynik:

```
Temperatura w Los Angeles wynosi: 22.0
```

## Wnikliwe spojrzenie
Historycznie, języki programowania takie jak C i C++ (od którego pochodzi składnia Arduino) nie posiadały wbudowanych tablic asocjacyjnych, co prowadziło do szukania obejść, takich jak powyżej przedstawione. To podejście jest stosunkowo proste, ale skaluje się źle, gdy rozmiar danych wzrasta, z powodu jego czasu wyszukiwania O(n).

Języki takie jak Python oferują słowniki, a JavaScript ma obiekty do tego celu, które są o wiele bardziej wydajne w zarządzaniu parami klucz-wartość. W Arduino, gdy wydajność i efektywność stają się kluczowe, programiści mogą optować za bardziej specjalistycznymi strukturami danych, takimi jak tablice mieszające, implementowane poprzez biblioteki.

Chociaż Arduino nie obsługuje rodzimie tablic asocjacyjnych, społeczność opracowała biblioteki takie jak `HashMap`, które można dodać do projektu, aby zapewnić podobną funkcjonalność z lepszą wydajnością niż podejście własne. Te biblioteki zazwyczaj oferują bardziej eleganckie i wydajne sposoby zarządzania tablicami asocjacyjnymi, szczególnie w bardziej złożonych projektach.
