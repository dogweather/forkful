---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:06:00.346637-07:00
description: "Jak to zrobi\u0107: PHP natywnie obs\u0142uguje r\xF3\u017Cne funkcje\
  \ do kapitalizacji ci\u0105g\xF3w znak\xF3w, z kt\xF3rych ka\u017Cda s\u0142u\u017C\
  y innemu celowi. Oto jak mo\u017Cesz ich u\u017Cywa\u0107: #."
lastmod: '2024-03-13T22:44:35.479304-06:00'
model: gpt-4-0125-preview
summary: "PHP natywnie obs\u0142uguje r\xF3\u017Cne funkcje do kapitalizacji ci\u0105\
  g\xF3w znak\xF3w, z kt\xF3rych ka\u017Cda s\u0142u\u017Cy innemu celowi."
title: "Zamiana liter na wielkie w \u0142a\u0144cuchu znak\xF3w"
weight: 2
---

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
