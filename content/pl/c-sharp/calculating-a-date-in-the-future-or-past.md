---
title:    "C#: Obliczanie daty w przyszłości lub przeszłości"
keywords: ["C#"]
---

{{< edit_this_page >}}

## Dlaczego?

Często zdarza się, że w programowaniu musimy wyliczać daty w przyszłości lub w przeszłości. Może to być potrzebne do tworzenia kalendarzy, rejestracji w systemach czy też dla celów statystycznych. W tym krótkim poradniku pokażę Wam, jak w prosty sposób obliczać daty w przyszłości lub w przeszłości w języku C#. 

## Jak to zrobić?

Obliczanie dat w przyszłości lub w przeszłości jest bardzo proste w języku C#. Służą do tego dwa główne mechanizmy - dodawanie lub odejmowanie określonej liczby dni od daty bazowej lub wykorzystanie klasy DateTime z wbudowanymi funkcjami do obliczania dat. 

### Dodawanie lub odejmowanie dni

Aby wyliczyć datę w przyszłości lub w przeszłości poprzez dodanie lub odejmowanie dni od daty bazowej, możemy skorzystać z metody AddDays() lub SubtractDays() klasy DateTime. Poniższy przykład pokazuje, jak wyliczyć datę 30 dni w przód od dzisiejszego dnia oraz jak wyliczyć datę 15 dni w przeszłości:

```C#
DateTime dzisiaj = DateTime.Today;
DateTime za30dni = dzisiaj.AddDays(30);
DateTime sprzed15dni = dzisiaj.SubtractDays(15);

Console.WriteLine("Data za 30 dni: " + za30dni.ToString("dd-MM-yyyy"));
Console.WriteLine("Data sprzed 15 dni: " + sprzed15dni.ToString("dd-MM-yyyy"));
```

Zwrócony zostanie tekst:

```
Data za 30 dni: 30-06-2021
Data sprzed 15 dni: 16-05-2021
```

### Używanie klasy DateTime

Klasa DateTime jest bardzo przydatna, jeśli chcemy wyliczyć datę w przyszłości lub w przeszłości na podstawie określonej daty bazowej. Wykorzystując metody takie jak AddYears(), AddMonths() czy AddHours(), możemy wyliczyć datę w dokładnie określonej dacie. W poniższym przykładzie pokazano, jak wyliczyć datę za rok od dzisiejszego dnia oraz jak wyliczyć datę 2 miesiące w przeszłości:

```C#
DateTime dzisiaj = DateTime.Today;
DateTime zaRok = dzisiaj.AddYears(1);
DateTime sprzed2miesiecy = dzisiaj.AddMonths(-2);

Console.WriteLine("Data za rok: " + zaRok.ToString("dd-MM-yyyy"));
Console.WriteLine("Data sprzed 2 miesięcy: " + sprzed2miesiecy.ToString("dd-MM-yyyy"));
```

Zwrócony zostanie tekst:

```
Data za rok: 15-05-2022
Data sprzed 2 miesięcy: 15-03-2021
```

## Deep Dive

Podczas korzystania z klasy DateTime należy pamiętać o kilku istotnych rzeczach. W przypadku dodawania lub odejmowania dni, możemy skorzystać także z metod AddHours(), AddMinutes(), AddSeconds() oraz AddMilliseconds(). O ile z reguły nie ma to większego znaczenia, to jednak podczas obliczania dat w przyszłości lub w przeszłości może być to istotne. Na przykład, jeśli dodamy 24 godziny do daty, otrzymamy datę o godzinie wstecz, ponieważ dodanie 24 godzin jest równoznaczne z przejściem do poprzedniego dnia.

Kolejną ważną rzeczą jest uwzględnienie roku przestępnego. W przypadku dodawania lub odejmowania lat, miesięcy i dni, klasa DateTime automatycznie uwzględnia rok przestępny i dostosowuje liczbę dni w roku. Jednak gdy korzystamy z metod takich jak AddDays(), AddHours() itp., musimy sami upewnić się, że uw