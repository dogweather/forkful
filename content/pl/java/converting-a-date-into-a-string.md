---
title:                "Konwersja daty na ciąg znaków"
html_title:           "Java: Konwersja daty na ciąg znaków"
simple_title:         "Konwersja daty na ciąg znaków"
programming_language: "Java"
category:             "Java"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/java/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## Co i dlaczego?

Konwertowanie daty na ciąg znaków jest powszechną operacją w programowaniu. Pozwala ona na zmianę wartości daty na czytelną dla człowieka postać, która może być wykorzystana na przykład w interfejsie użytkownika. Programiści często wykonują tę czynność w celu wizualnego przekazywania informacji o dacie.

## Jak to zrobić:

```Java
import java.text.SimpleDateFormat;
import java.util.Date;

public class DateFormatter {

	public static void main(String[] args) {
		
		// Utworzenie obiektu typu Date z aktualną datą
		Date date = new Date();
		
		// Utworzenie obiektu SimpleDateFormat z wybranym formatem
		SimpleDateFormat formatter = new SimpleDateFormat("dd.MM.yyyy");
		
		// Wyświetlenie daty w postaci ciągu znaków
		System.out.println(formatter.format(date));
		
		// Możliwe formaty:
		// "dd.MM.yyyy" - 01.01.2019
		// "dd.MM.yyyy HH:mm:ss" - 01.01.2019 14:30:00
		// "dd.MM.yyyy, EEEE" - 01.01.2019, wtorek
	}

}
```

Oto wynik działania powyższego kodu:

```
24.02.2021
```

## Wnikliwa analiza:

**Kontekst historyczny:**

Konwertowanie dat na ciągi znaków jest wykorzystywane od lat w różnych językach programowania. W Java pojawiło się to już w pierwszej wersji - Java 1.0. Nitka formatowania została wprowadzona w celu ułatwienia tworzenia czytelnego kodu.

**Alternatywy:**

Alternatywnymi sposobami na konwertowanie daty na ciąg znaków są m.in. biblioteki do obsługi dat, takie jak Joda-Time lub Java 8 Date and Time API.

**Szczegóły implementacji:**

W Java do konwertowania daty na ciąg znaków wykorzystywany jest obiekt SimpleDateFormat. Służy on do formatowania i analizy dat w danym formacie. Wymaga podania odpowiedniej maski, która określa sposób prezentacji daty.

## Zobacz także:

Dokumentacja klasy ```SimpleDateFormat```: https://docs.oracle.com/javase/8/docs/api/java/text/SimpleDateFormat.html