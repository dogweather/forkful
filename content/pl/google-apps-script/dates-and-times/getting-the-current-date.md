---
changelog:
- 2024-02-01, gpt-4-0125-preview, translated from English
date: 2024-02-01 21:54:44.083688-07:00
description: "Pobieranie bie\u017C\u0105cej daty w Google Apps Script dotyczy uzyskania\
  \ aktualnej daty i godziny, co jest powszechnym zadaniem przy automatyzacji zada\u0144\
  ,\u2026"
lastmod: '2024-03-11T00:14:08.085716-06:00'
model: gpt-4-0125-preview
summary: "Pobieranie bie\u017C\u0105cej daty w Google Apps Script dotyczy uzyskania\
  \ aktualnej daty i godziny, co jest powszechnym zadaniem przy automatyzacji zada\u0144\
  ,\u2026"
title: Pobieranie aktualnej daty
---

{{< edit_this_page >}}

## Co i dlaczego?

Pobieranie bieżącej daty w Google Apps Script dotyczy uzyskania aktualnej daty i godziny, co jest powszechnym zadaniem przy automatyzacji zadań, rejestrowaniu i znakowaniu czasu w aplikacjach związanych z ekosystemem Google. Programiści używają tego do generowania dynamicznych treści, śledzenia terminów i planowania w Google Docs, Arkuszach i innych usługach Google.

## Jak to zrobić:

Google Apps Script, który bazuje na JavaScript, oferuje proste metody na uzyskanie bieżącej daty. Możesz użyć konstruktora `new Date()` do stworzenia nowego obiektu daty, reprezentującego bieżącą datę i godzinę. Oto jak można manipulować i wyświetlać to w różnych formatach.

```javascript
function showCurrentDate() {
  var currentDate = new Date();
  
  Logger.log(currentDate); // Rejestruje bieżącą datę i godzinę w strefie czasowej skryptu
  
  // Aby wyświetlić tylko datę w formacie RRRR-MM-DD
  var dateString = currentDate.getFullYear() + '-' + 
                   (currentDate.getMonth() + 1).toString().padStart(2, '0') + '-' + 
                   currentDate.getDate().toString().padStart(2, '0');
  Logger.log(dateString); // Przykładowy wynik: "2023-04-01"
  
  // Wyświetlanie w bardziej czytelnym formacie
  var options = { year: 'numeric', month: 'long', day: 'numeric', hour: '2-digit', minute: '2-digit', second: '2-digit', timeZoneName: 'short' };
  var readableDate = currentDate.toLocaleDateString('en-US', options) + ' ' + 
                     currentDate.toLocaleTimeString('en-US', options);
                     
  Logger.log(readableDate); // Przykładowy wynik: "April 1, 2023, 12:00:00 PM GMT+1"
}
```

Te fragmenty kodu demonstrują, jak przechwycić i sformatować bieżącą datę i godzinę, pokazując wszechstronność dla różnych potrzeb programistycznych w Google Apps Script.

## Dogłębna analiza

Zanim JavaScript ustalił obiekt `Date`, programiści musieli ręcznie śledzić czas i datę przez mniej standardowe i bardziej uciążliwe sposoby. Obejmowało to używanie liczb całkowitych znacznika czasu i własnych funkcji daty, które różniły się w zależności od środowiska programistycznego, prowadząc do niespójności i problemów z kompatybilnością.

Wprowadzenie obiektu `new Date()` w JavaScript, a przez to w Google Apps Script, ustandaryzowało operacje związane z datą i czasem, czyniąc je bardziej intuicyjnymi i redukując ilość kodu potrzebnego do operacji związanych z datą. Warto zauważyć, że choć implementacja w Google Apps Script jest wygodna i wystarczająca dla wielu aplikacji w zestawie produktów Google, może nie zaspokajać wszystkich scenariuszy, szczególnie tych wymagających skomplikowanego obsługiwania stref czasowych lub precyzyjnego rejestrowania znaczników czasu w środowiskach o szybkim tempie.

Dla tak zaawansowanych przypadków użytkowania programiści często zwracają się do bibliotek takich jak Moment.js czy date-fns w JavaScript. Chociaż Google Apps Script nie obsługuje natywnie tych bibliotek, deweloperzy mogą naśladować niektóre z ich funkcjonalności, używając dostępnych metod JavaScript Date lub poprzez dostęp do zewnętrznych bibliotek za pomocą usługi HTML Service lub URL Fetch service w Apps Script. Pomimo tych alternatyw, prostota i integracja rodzimych funkcji daty i czasu w Google Apps Script pozostają podstawowym rozwiązaniem dla większości zadań w ekosystemie Google.
