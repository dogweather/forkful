---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:06:00.346637-07:00
description: "Du\u017Ca litera na pocz\u0105tku ci\u0105gu znak\xF3w polega na zmodyfikowaniu\
  \ pierwszego znaku danego tekstu na wielk\u0105 liter\u0119, zapewniaj\u0105c, \u017C\
  e zdania, tytu\u0142y lub nazwy\u2026"
lastmod: '2024-03-13T22:44:35.479304-06:00'
model: gpt-4-0125-preview
summary: "Du\u017Ca litera na pocz\u0105tku ci\u0105gu znak\xF3w polega na zmodyfikowaniu\
  \ pierwszego znaku danego tekstu na wielk\u0105 liter\u0119, zapewniaj\u0105c, \u017C\
  e zdania, tytu\u0142y lub nazwy w\u0142asne zaczynaj\u0105 si\u0119 poprawnie w\
  \ zbiorze danych."
title: "Zamiana liter na wielkie w \u0142a\u0144cuchu znak\xF3w"
weight: 2
---

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
