---
title:                "Pisanie pliku tekstowego"
html_title:           "Arduino: Pisanie pliku tekstowego"
simple_title:         "Pisanie pliku tekstowego"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/arduino/writing-a-text-file.md"
---

{{< edit_this_page >}}

## Dlaczego

Pisanie pliku tekstowego jest bardzo przydatną umiejętnością w programowaniu Arduino. Dzięki temu możemy przechowywać dane i informacje w sposób uporządkowany, co ułatwia nam pracę z naszym projektem. Dodatkowo, korzystanie z plików tekstowych jest niezbędne przy pracy z bardziej zaawansowanymi funkcjonalnościami takimi jak komunikacja przez sieć, czy zapis i odczyt danych z pamięci zewnętrznej.

## Jak napisać plik tekstowy w Arduino

Aby napisać plik tekstowy w Arduino, musimy najpierw skorzystać z biblioteki "SD". Następnie należy utworzyć obiekt dla karty SD i otworzyć plik, którego chcemy użyć za pomocą funkcji "open()". Wewnątrz funkcji "open()" musimy podać nazwę pliku oraz tryb, w jakim będziemy z niego korzystać, na przykład "FILE_WRITE" dla zapisu lub "FILE_READ" dla odczytu. Po utworzeniu pliku możemy go modyfikować za pomocą funkcji "print()", "println()" lub "write()". Na koniec musimy zapisać zmiany w pliku przy użyciu funkcji "close()".

```Arduino
#include <SD.h> //importowanie biblioteki

File myfile; //deklaracja obiektu pliku

void setup() {
  Serial.begin(9600);

  //inicjalizacja karty SD
  if (!SD.begin(4)) {
    Serial.println("Błąd przy inicjalizacji karty SD");
    return;
  }

  //otwarcie pliku tekstowego o nazwie "dane.txt" w trybie zapisu
  myfile = SD.open("dane.txt", FILE_WRITE);
  
  //dodanie tekstu do pliku
  myfile.print("Dane: ");
  myfile.println("12345");
  myfile.write("Liczba: ");
  myfile.println(67890);

  //zapisanie zmian i zamknięcie pliku
  myfile.close();
}

void loop() {
  
}
```

Po wykonaniu tego kodu, na karcie SD pojawi się plik "dane.txt" z zawartością:

Dane: 12345
Liczba: 67890


## Deep Dive

W przypadku bardziej zaawansowanych scenariuszy, możemy również używać specjalnych znaków, takich jak "\n" (nowa linia), "\t" (tabulator) czy "\r" (powrót karetki) w celu formatowania tekstu w pliku. Dodatkowo, możemy też tworzyć i korzystać z folderów na karcie SD.

Istnieje również możliwość zapisania i odczytania danych w formacie CSV (Comma-Separated Values), który jest powszechnie stosowany do przechowywania i wymiany danych między różnymi aplikacjami. Aby zapisać dane w tym formacie, musimy przed każdym elementem oddzielić go przecinkiem, na przykład:

```Arduino
//zapisanie danych w formacie CSV
myfile.print("Dane:"); //tutaj można również użyć funkcji println()
myfile.print(","); //oddzielenie elementów przecinkiem
myfile.println("12345"); 
```

Aby odczytać te dane, możemy skorzystać z funkcji "readStringUntil()" w pętli "while":

```Arduino
void setup() {
  //pierwsza część kodu pozostaje niezmieniona

  //otwarcie pliku tekstowego o nazwie "dane.txt" w trybie odczytu
  //uwaga, że jest to inny tryb niż w poprzednim przykładzie
  myfile = SD.open("dane.txt", FILE_READ);

  String data = ""; //utworzenie zmiennej tekstowej do przechowywania danych odczytanych z pliku
  while (myfile.available()) {
    data = myfile.readStringUntil(','); //odczytanie danych do znaku przecinka
    Serial