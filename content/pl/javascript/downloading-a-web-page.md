---
title:                "Javascript: Pobieranie strony internetowej"
simple_title:         "Pobieranie strony internetowej"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/javascript/downloading-a-web-page.md"
---

{{< edit_this_page >}}

## Dlaczego pobieranie strony internetowej jest ważne?

Pobieranie strony internetowej jest bardzo ważną częścią codziennej pracy programistów. Pozwala ono na wyświetlenie danych, treści i grafik w przystępnej formie dla użytkownika. Bez tego procesu, użytkownicy nie mogliby wygodnie korzystać z internetu.

## Jak to zrobić?

Kodowanie pobierania strony internetowej może wydawać się trudne, ale jest to w rzeczywistości bardzo proste dzięki językowi programowania Javascript. Korzystając z prostych metod i funkcji, możesz pobrać stronę internetową i wyświetlić jej zawartość w kilku prostych krokach.

```Javascript
// Definicja funkcji pobierającej stronę internetową
function pobierzStrone(linkStrony) {
   var request = new XMLHttpRequest(); // Tworzenie nowego obiektu XMLHttpRequest
   request.open('GET', linkStrony, true); // Otwarcie połączenia z linkiem strony
   request.onload = function() { // Reakcja na załadowanie strony
      if (this.status >= 200 && this.status < 400) {
         // Jeśli pobieranie było udane (status pomiędzy 200 a 400)
         var data = this.response; // Zwrócenie pobranych danych jako tekstu
         console.log(data); // Wyświetlenie danych w konsoli
      } else {
         // Jeśli wystąpił błąd
         console.log('Błąd pobierania strony.');
      }
   };
   request.send(); // Wysłanie zapytania i pobranie strony
}

// Wywołanie funkcji z linkiem do strony
pobierzStrone('https://www.example.com');
```

## Głębszy wgląd

Powyższy kod pokazuje prosty sposób na pobranie strony internetowej. Jednak w przypadku bardziej skomplikowanych stron, może być konieczne wykorzystanie dodatkowych funkcji, takich jak przetwarzanie danych otrzymanych z serwera lub obsługa różnych typów odpowiedzi. Warto także pamiętać o zabezpieczeniach, takich jak uwierzytelnianie, gdy pobierasz strony internetowe z ograniczonym dostępem.

## Zobacz również

- [Dokumentacja języka Javascript](https://developer.mozilla.org/pl/docs/Web/JavaScript)
- [Poradnik dla początkujących w pobieraniu danych z internetu](https://www.w3schools.com/xml/xml_http.asp)
- [Przykład wykorzystania pobierania strony w projekcie](https://github.com/example/pobieranie-strony)