---
title:                "Odczytywanie argumentów wiersza poleceń"
html_title:           "Arduino: Odczytywanie argumentów wiersza poleceń"
simple_title:         "Odczytywanie argumentów wiersza poleceń"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/arduino/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## Dlaczego

Dlaczego ktoś chciałby poczytać o argumentach wiersza poleceń? W prosty sposób ten proces pomaga programiście uzyskać dane bezpośrednio z konsoli, co może być przydatne w różnych sytuacjach, takich jak testowanie lub konfiguracja kodu.

## Jak to zrobić

Aby odczytać argumenty wiersza poleceń w Arduino, musimy skorzystać z obiektu **Serial**. Najpierw musimy zainicjować obiekt, a następnie użyć funkcji **Serial.read()**, aby odczytać pojedynczy znak z konsoli. Możemy również użyć funkcji **Serial.parseInt()**, aby odczytać argumenty liczbowe. Poniższy kod pokazuje przykład odczytania argumentów wiersza poleceń i wykorzystania ich w programie:
```Arduino
void setup(){
  Serial.begin(9600); //inicjalizacja obiektu Serial
}

void loop(){
  if (Serial.available()){ //sprawdzenie, czy są dostępne dane w konsoli
    int arg = Serial.parseInt(); //odczytanie argumentu liczbowego
    Serial.print("Odczytany argument to: ");
    Serial.println(arg);
  }
}
```

Poniżej znajduje się przykład z użyciem tekstu jako argumentu:
```Arduino
void setup(){
  Serial.begin(9600);
}

void loop(){
  if (Serial.available()){
    String arg = Serial.readString(); //odczytanie argumentu jako tekstu
    Serial.print("Odczytany argument to: ");
    Serial.println(arg);
  }
}
```

W konsoli należy wpisać argument, a następnie zakończyć go znakiem nowej linii (enter). Wyprintuje on odczytany argument w monitorze szeregowym.

## Głębsza analiza

Inną funkcją, która może być użyteczna przy odczytywaniu argumentów wiersza poleceń, jest **Serial.readStringUntil()**. Pozwala ona określić znak, który będzie kończył odczytywany tekst. Przykładowo, za pomocą kodu "Serial.readStringUntil(',')", odczytywany tekst zostanie zakończony przecinkiem. Możemy również użyć funkcji **Serial.find()**, aby pozwolić na odczytanie tylko części tekstu. Przykład z użyciem obu funkcji:
```Arduino
void setup(){
  Serial.begin(9600);
}

void loop(){
  if (Serial.available()){
    String arg = Serial.readStringUntil(','); //odczytywanie tekstu do przecinka
    if (arg == "Hello"){ //sprawdzenie, czy odczytany tekst zawiera słowo "Hello"
      Serial.println("Witaj użytkowniku!");
    }
  }
}
```

Wydajemy polecenie "Serial.println("Hello, World!")" z konsoli. W odpowiedzi otrzymamy "Witaj użytkowniku!" w polu monitora szeregowego.

## Zobacz także

Aby dowiedzieć się więcej o obsłudze obiektu Serial, zapoznaj się z dokumentacją Arduino ([https://www.arduino.cc/reference/en/language/functions/communication/serial/](https://www.arduino.cc/reference/en/language/functions/communication/serial/))

Aby zobaczyć więcej przykładów użycia argumentów wiersza poleceń w Arduino, przejrzyj wpisy na forum dla społeczności Arduino ([https://forum.arduino.cc/](https://forum.arduino.cc/)) lub odwiedź stronę z projektami Arduino ([https://create.arduino.cc/projecthub](https://create.arduino.cc/projecthub)).