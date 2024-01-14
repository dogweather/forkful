---
title:    "Ruby: Uzyskiwanie bieżącej daty"
keywords: ["Ruby"]
---

{{< edit_this_page >}}

## Dlaczego

W dzisiejszych czasach coraz więcej ludzi uczęszcza na kursy programowania, a nie ma w tym nic dziwnego. Programowanie jest nie tylko ciekawym i kreatywnym zajęciem, ale także zawodem przyszłości. Jednym z pierwszych kroków w nauce programowania jest poznanie podstawowych funkcji i możliwości języka programowania. Dlatego też w dzisiejszym wpisie zajmiemy się jedną z nich - jak uzyskać bieżącą datę w języku Ruby.

## Jak to zrobić?

Do uzyskania aktualnej daty w języku Ruby możemy wykorzystać klasę `Date`. Przykładowo, jeśli chcemy wyświetlić obecną datę w konsoli, należy wprowadzić następujący kod:

```Ruby
puts Date.today
```
W wyniku otrzymamy datę w formacie `yyyy-mm-dd`.

Jeśli chcemy wyświetlić nazwę dnia tygodnia oraz miesiąca, możemy to zrobić korzystając z metod `strftime` oraz `localize`:

```Ruby
puts Date.today.strftime("%A, %B").localize
```
Dzięki temu uzyskamy datę w formacie `nazwa dnia tygodnia, nazwa miesiąca`.

## Wnikliwa analiza

Przypuśćmy, że chcemy otrzymać informację o dacie w danym formacie, ale z innej strefy czasowej. W takiej sytuacji możemy wykorzystać klasę `DateTime`, która posiada możliwość ustawiania różnych stref czasowych. Przykładowy kod może wyglądać następująco:

```Ruby
puts DateTime.now.new_offset(3)
```
W tym przypadku, ustawiliśmy strefę czasową na GMT+3, co spowoduje wyświetlenie bieżącej daty z uwzględnieniem tej zmiany.

Ważną klasą jest również `Time`, która pozwala na manipulowanie czasem, np. dodawanie lub odejmowanie dni, godzin czy minut. Może to być szczególnie przydatne w przypadku programowania aplikacji, gdzie wymagane jest zarządzanie czasem.

## Zobacz także

- [Dokumentacja Ruby o klasie Date](https://ruby-doc.org/stdlib-2.6.3/libdoc/date/rdoc/Date.html)
- [Przewodnik przez klasę DateTime w języku Ruby](https://www.rubyguides.com/2018/06/ruby-datetime/)
- [Wykorzystanie klasy Time w ustawianiu czasu](https://stackoverflow.com/questions/21183069/set-time-from-string-with-timezone-in-ruby)