---
title:    "Java: Napisanie pliku tekstowego"
keywords: ["Java"]
---

{{< edit_this_page >}}

## Dlaczego

Pisanie plików tekstowych jest nieodzownym elementem programowania w Javie. Pliki tekstowe pozwalają na trwałe przechowywanie i udostępnianie różnego rodzaju danych. Są one niezbędne w tworzeniu aplikacji, które wymagają zapisywania i odczytywania informacji.

## Jak to zrobić

Aby rozpocząć pisanie pliku tekstowego w Javie, należy najpierw utworzyć obiekt typu FileWriter, który będzie służył do zapisywania danych w pliku. Następnie należy przekazać nazwę pliku, do którego chcemy zapisać dane, jako parametr w konstruktorze.

```Java
FileWriter writer = new FileWriter("plik.txt");
```

Kolejnym krokiem jest utworzenie obiektu typu BufferedWriter, który będzie służył do zapisywania danych do obiektu FileWriter. W tym przypadku należy przekazać jako parametr obiekt FileWriter.

```Java
BufferedWriter bWriter = new BufferedWriter(writer);
```

Teraz możemy korzystać z metod dostępnych w obiekcie BufferedWriter, takich jak write (), aby zapisać dane do pliku. Należy pamiętać, że każde wywołanie metody write () musi zostać zakończone wywołaniem metody newLine (), aby przesunąć kursor do nowej linii.

```Java
bWriter.write("To jest przykładowa linia tekstu.");
bWriter.newLine();
```

Po zapisaniu wszystkich danych, należy zamknąć obiekt BufferedWriter za pomocą metody close (), co również spowoduje zamknięcie obiektu FileWriter i zapisanie zmian w pliku.

```Java
bWriter.close();
```

Aby odczytać dane z pliku tekstowego, można wykorzystać obiekt FileReader i BufferedReader w podobny sposób. Pierwszy będzie służył do odczytu danych, a drugi do ich bufferowania, co przyspieszy proces odczytu.

## Deep Dive

Pliki tekstowe mogą być szeroko wykorzystywane w programowaniu w Javie. Mogą one zawierać dane w różnych formatach, takich jak tekst, liczby czy nawet obiekty. W celu zapisania obiektów do pliku, można wykorzystać mechanizm serializacji, który przekształci obiekt w strumień bajtów i umożliwi zapisanie go do pliku.

Dodatkowo, pliki tekstowe mogą być również wykorzystywane do przechowywania konfiguracji aplikacji lub do przechowywania logów z jej działania. W przypadku przechowywania konfiguracji, należy pamiętać o wykorzystaniu odpowiedniego formatu, takiego jak JSON czy XML, aby umożliwić łatwe odczytywanie i interpretowanie danych.

## Zobacz również

- [Java FileWriter i BufferedWriter - przykłady użycia](https://javastart.pl/kurs/java-io/java-bufferedwriter/)
- [Praca z plikami w Javie - przykłady użycia](https://www.samouczekprogramisty.pl/praca-z-plikami-w-javie/)
- [Serializacja obiektów w Javie](https://www.javatpoint.com/serialization-in-java)