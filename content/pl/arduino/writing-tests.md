---
title:    "Arduino: Pisanie testów"
keywords: ["Arduino"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/pl/arduino/writing-tests.md"
---

{{< edit_this_page >}}

## Dlaczego warto testować kod w Arduino?

Testowanie kodu jest kluczowym elementem w procesie pisania programów dla Arduino. Dzięki testom można upewnić się, że nasz kod działa poprawnie i uniknąć problemów w przyszłości. Jest to również ważne dla osób korzystających z naszego kodu, ponieważ testy dają pewność, że nasz projekt działa zgodnie z oczekiwaniami.

## Jak testować kod w Arduino?

Aby rozpocząć testowanie kodu w Arduino, należy wykonać kilka kroków:

1. Otwórz nowy projekt w Arduino IDE i nazwij go "Testowanie Kodu".
2. Utwórz nowy plik z funkcją testującą i nazwij go "tests.ino".
3. W pliku "tests.ino" umieść funkcję, którą chcesz przetestować, np. zmienną i funkcję odpowiadającą za jej zmianę.
4. Następnie stwórz funkcję testującą, która będzie sprawdzać poprawność działania naszej zmiennej i jej funkcji.
5. Skompiluj i przetestuj kod, upewniając się, że funkcja testująca wykrywa ewentualne problemy.

Przykładowy kod wyglądałby następująco:

```Arduino
// funkcja odpowiadająca za zmianę wartości zmiennej
int zmienWartosc(int x){
  return x + 10; 
} 
// Funkcja testująca
void testoweFunkcje(){
  int testowaZmienna = 0; 
  testowaZmienna = zmienWartosc(testowaZmienna); 
  if(testowaZmienna != 10){
    // wypisz błąd
    Serial.println("Błąd: Wartość zmiennej nie została zmieniona!");
  }
}
```

### Pamiętaj o poprawnym oznaczeniu pinów

Podczas testowania kodu w Arduino ważne jest, aby pamiętać o poprawnym oznaczeniu pinów, szczególnie jeśli korzystamy z płytki z mikrokontrolerem o ograniczonej ilości pinów. W tym celu można wykorzystać funkcję "pinMode", która ustawia tryb pracy pinu jako wejście lub wyjście.

## Głębszy zanurzenie w pisaniu testów

Pisanie testów dla kodu w Arduino może być złożonym procesem, szczególnie jeśli projekt jest rozbudowany i wykorzystuje wiele funkcji. Ważne jest, aby pamiętać o dokładnym sprawdzaniu poprawności każdej funkcji, aby uniknąć ewentualnych błędów w przyszłości.

Jedną z zalet pisania testów jest możliwość wczesnego wykrywania błędów i szybszego ich naprawiania. W przypadku rozbudowanych projektów testowanie kodu może pomóc w zlokalizowaniu problemu, co ułatwia jego naprawę.

## Zobacz również

Jeśli jesteś zainteresowany głębszym poznaniem tematu testowania kodu w Arduino, polecamy zapoznanie się z poniższymi artykułami:

- [Oficjalna dokumentacja testowania kodu w Arduino](https://www.arduino.cc/en/Reference/ArduinoUnit)
- [Narzędzia do testowania kodu w Arduino](https://www.circuits.dk/tools-for-testing-your-arduino-sketch-routines/)
- [Jak pisać testy dla kodu w Arduino](https://www.youtube.com/watch?v=SQmLtH9MpyA) (wideo)