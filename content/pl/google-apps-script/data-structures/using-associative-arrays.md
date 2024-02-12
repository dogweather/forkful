---
title:                "Korzystanie z tablic asocjacyjnych"
date:                  2024-02-01T22:04:22.869553-07:00
model:                 gpt-4-0125-preview
simple_title:         "Korzystanie z tablic asocjacyjnych"
tag:                  "Data Structures"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/google-apps-script/using-associative-arrays.md"
changelog:
  - 2024-02-01, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Co i dlaczego?

Tablice asocjacyjne, znane jako obiekty w Google Apps Script (odmiana JavaScript), umożliwiają programistom tworzenie kolekcji par klucz-wartość. Funkcjonalność ta jest kluczowa do przechowywania i manipulowania danymi w sposób efektywny, szczególnie przy pracy z dynamicznie nazwanymi właściwościami lub gdy linearny model przechowywania i dostępu tradycyjnej tablicy jest niewystarczający.

## Jak to zrobić:

W Google Apps Script tworzy się i manipuluje tablicami asocjacyjnymi (obiektami) za pomocą nawiasów klamrowych `{}`, definiując w nich pary klucz-wartość. Klucze są unikalnymi identyfikatorami, a wartości mogą być czymś od ciągów znaków i liczb po bardziej złożone obiekty lub funkcje. Oto podstawowy przykład:

```javascript
function createAssociativeArray() {
  var user = {
    name: "John Doe",
    age: 30,
    email: "johndoe@example.com"
  };

  // Dostęp do wartości
  Logger.log(user.name); // Wyświetla: John Doe
  Logger.log(user["email"]); // Wyświetla: johndoe@example.com

  // Dodawanie nowych par klucz-wartość
  user.title = "Software Developer";
  user["country"] = "USA";

  Logger.log(user.title); // Wyświetla: Software Developer

  // Iteracja po parach klucz-wartość
  for (var key in user) {
    Logger.log(key + ': ' + user[key]);
  }
}
```

Przykładowe wyjście dla części iteracyjnej może wyglądać tak:
```
name: John Doe
age: 30
email: johndoe@example.com
title: Software Developer
country: USA
```

Zwróć uwagę, jak można używać zarówno notacji kropkowej, jak i nawiasowej do dostępu i ustawiania właściwości. Notacja nawiasowa jest szczególnie przydatna przy pracy z kluczami, które są dynamicznie określane lub zawierają znaki niedozwolone w identyfikatorach.

## W głąb

Tablice asocjacyjne w formie obiektów stanowiły kamień węgielny JavaScriptu, a przez to Google Apps Script, odzwierciedlając jego mechanizm dziedziczenia oparty na prototypach. W przeciwieństwie do języków z tradycyjnymi tablicami asocjacyjnymi lub słownikami (np. dict w Pythonie), obiekty Google Apps Script zapewniają elastyczne i potężne środki strukturyzacji danych, korzystając z dynamicznej natury JavaScriptu.

Warto jednak zauważyć, że specyfikacja ECMAScript 2015 wprowadziła obiekty `Map` i `Set`, oferując bardziej bezpośrednie obsługiwanie kolekcji asocjacyjnych z pewnymi korzyściami w porównaniu z obiektami, takimi jak utrzymanie kolejności wstawiania i lepsza wydajność dla dużych zbiorów danych. Chociaż Google Apps Script również wspiera te struktury, wybór między używaniem obiektów a nowszymi strukturami `Map`/`Set` zależy od konkretnych potrzeb i rozważań dotyczących wydajności. Dla większości zadań związanych z tablicami asocjacyjnymi, tradycyjne implementacje oparte na obiektach zapewniają znajome i wszechstronne podejście, ale warto rozważyć nowsze alternatywy w miarę wzrostu złożoności skryptu.
