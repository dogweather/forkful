---
title:                "Zamiana liter na wielkie w łańcuchu znaków"
date:                  2024-02-03T19:06:00.346637-07:00
model:                 gpt-4-0125-preview
simple_title:         "Zamiana liter na wielkie w łańcuchu znaków"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/php/capitalizing-a-string.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Co i dlaczego?
Duża litera na początku ciągu znaków polega na zmodyfikowaniu pierwszego znaku danego tekstu na wielką literę, zapewniając, że zdania, tytuły lub nazwy własne zaczynają się poprawnie w zbiorze danych. Programiści często wykonują kapitalizację ciągu znaków w celu normalizacji danych, poprawy czytelności lub zapewnienia spójności w danych wejściowych użytkownika lub przetwarzaniu danych tekstowych.

## Jak to zrobić:
PHP natywnie obsługuje różne funkcje do kapitalizacji ciągów znaków, z których każda służy innemu celowi. Oto jak możesz ich używać:

### Kapitalizacja pierwszej litery ciągu:

```php
$string = "cześć, świecie!";
$capitalizedString = ucfirst($string);
echo $capitalizedString; // Wyświetla: Cześć, świecie!
```

### Kapitalizacja pierwszej litery każdego słowa:

```php
$string = "cześć, świecie!";
$capitalizedWords = ucwords($string);
echo $capitalizedWords; // Wyświetla: Cześć, Świecie!
```

### Konwersja całego ciągu na wielkie litery:

```php
$string = "cześć, świecie!";
$upperCaseString = strtoupper($string);
echo $upperCaseString; // Wyświetla: CZEŚĆ, ŚWIECIE!
```

W scenariuszach wymagających większej personalizacji lub rozwiązań stron trzecich, można wykorzystać biblioteki takie jak `mbstring` (dla ciągów wielobajtowych), szczególnie przy pracy z internacjonalizacją, gdzie znaki mogą wykraczać poza podstawowy zestaw ASCII.

### Użycie mbstring do kapitalizacji ciągów UTF-8:

Upewnij się, że masz włączone rozszerzenie `mbstring` w konfiguracji PHP, a następnie:

```php
$string = "élégant";
$capitalizedString = mb_convert_case($string, MB_CASE_TITLE, "UTF-8");
echo $capitalizedString; // Wyświetla: Élégant
```

To podejście pomaga dokładnie kapitalizować ciągi zawierające znaki nie-ASCII, przestrzegając niuansów różnych języków.
