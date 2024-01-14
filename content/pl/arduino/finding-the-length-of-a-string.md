---
title:    "Arduino: Znalezienie długości ciągu znaków"
keywords: ["Arduino"]
---

{{< edit_this_page >}}

## Dlaczego

Znajdywanie długości łańcucha jest ważnym aspektem programowania Arduino. Pozwala to na inteligentne zarządzanie pamięcią i efektywne wykorzystanie zasobów układu. W tym artykule dowiesz się, dlaczego warto nauczyć się znajdywać długość łańcucha oraz jak to zrobić w prosty sposób.

## Jak to zrobić

Aby znaleźć długość łańcucha w Arduino, musisz użyć funkcji `length()`. Poniższy przykład kodu pokazuje, jak można ją wykorzystać w praktyce:

```arduino
void setup() {
  Serial.begin(9600);
  String name = "Arduino";
  int length = name.length();
  Serial.print("Długość łańcucha " + name + " to: ");
  Serial.println(length);
}

void loop() {
  // puste pole
}
```

Po wgraniu tego kodu na płytkę Arduino i otwarciu monitora szeregowego, powinieneś zobaczyć następujący wynik:

```
Długość łańcucha Arduino to: 7
```

Jak widać, funkcja `length()` zwraca liczbę znaków w podanym łańcuchu. Możesz także wykorzystać ją do potrzeb obliczeń lub porównywania długości różnych łańcuchów.

## Deep Dive

Wewnątrz funkcji `length()` używane są wskaźniki, które odnoszą się do początku i końca łańcucha. Następnie jest obliczana różnica pomiędzy nimi, co daje długość łańcucha. Warto pamiętać, że funkcja ta zwraca typ `size_t`, który jest równoważny do `unsigned int`, więc może wystąpić problem w przypadku napotkania bardzo długich łańcuchów.

## Zobacz także

- Dokumentacja Arduino: https://www.arduino.cc/reference/en/language/variables/data-types/string/functions/length/ 
- Video tutorial o znajdowaniu długości łańcucha: https://www.youtube.com/watch?v=F6Wb65dwrLI
- Przydatne funkcje do pracy z łańcuchami w Arduino: https://lastminuteengineers.com/working-with-strings-arduino/