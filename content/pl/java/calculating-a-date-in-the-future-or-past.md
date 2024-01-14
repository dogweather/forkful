---
title:                "Java: Obliczanie daty w przyszłości lub przeszłości"
simple_title:         "Obliczanie daty w przyszłości lub przeszłości"
programming_language: "Java"
category:             "Java"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/java/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## Dlaczego

Obliczanie daty w przyszłości lub przeszłości może być bardzo przydatne w programowaniu, szczególnie w przypadku tworzenia aplikacji, które muszą śledzić terminy lub okresy czasu. Może to również być przydatne podczas tworzenia narzędzi do planowania lub organizowania zadań w przyszłości.

## Jak to zrobić

Aby obliczyć datę w przyszłości lub przeszłości, należy użyć klasy "Calendar" z pakietu "java.util". Poniższy przykład pokazuje, jak użyć tej klasy w celu obliczenia daty dziesięć dni w przyszłości:

```Java
Calendar calendar = Calendar.getInstance();
calendar.add(Calendar.DATE, 10);
System.out.println("Data dziesięć dni w przyszłości: " + calendar.getTime());
```

Wyjście:

```
Data dziesięć dni w przyszłości: Tue Nov 17 11:41:34 EST 2020
```

W powyższym przykładzie najpierw tworzona jest instancja klasy "Calendar" z użyciem metody "getInstance()". Następnie za pomocą metody "add()" dodajemy 10 dni do aktualnej daty. Na koniec, wyświetlamy wynik za pomocą metody "getTime()".

## Wnikliwe zagłębienie

Klasa "Calendar" zawiera wiele innych metod, które umożliwiają bardziej precyzyjne obliczenia daty w przyszłości lub przeszłości. Na przykład, można użyć metody "set()" do ustawienia konkretnego dnia, miesiąca i roku. Można również użyć metod "roll()" lub "setTime()" do manipulowania datą i czasem na poziomie pól, takich jak godzina, minuta lub sekunda.

Inną przydatną klasą jest "Date", która pozwala na obliczanie daty w różnych strefach czasowych. Można także użyć klasy "SimpleDateFormat" do formatowania daty w określonym formacie.

Zachęcamy do dokładnego zapoznania się z dokumentacją klas "Calendar" i "Date" oraz eksperymentowania z różnymi metodami, aby dostosować obliczenia daty według indywidualnych potrzeb.

## Zobacz również

- Dokumentacja klasy Calendar: https://docs.oracle.com/javase/8/docs/api/java/util/Calendar.html
- Dokumentacja klasy Date: https://docs.oracle.com/javase/8/docs/api/java/util/Date.html
- Przewodnik po klasie SimpleDateFormat: https://docs.oracle.com/javase/8/docs/api/java/text/SimpleDateFormat.html