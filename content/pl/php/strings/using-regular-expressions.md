---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:17:40.363259-07:00
description: "Wyra\u017Cenia regularne (regex) w PHP to wzorce u\u017Cywane do dopasowywania\
  \ kombinacji znak\xF3w w ci\u0105gach, co pozwala na zaawansowane operacje wyszukiwania\
  \ i\u2026"
lastmod: 2024-02-19 22:04:54.620716
model: gpt-4-0125-preview
summary: "Wyra\u017Cenia regularne (regex) w PHP to wzorce u\u017Cywane do dopasowywania\
  \ kombinacji znak\xF3w w ci\u0105gach, co pozwala na zaawansowane operacje wyszukiwania\
  \ i\u2026"
title: "Korzystanie z wyra\u017Ce\u0144 regularnych"
---

{{< edit_this_page >}}

## Co i dlaczego?

Wyrażenia regularne (regex) w PHP to wzorce używane do dopasowywania kombinacji znaków w ciągach, co pozwala na zaawansowane operacje wyszukiwania i zamiany oraz walidację danych. Programiści wykorzystują regex ze względu na jego moc i elastyczność w analizowaniu tekstu, walidacji formularzy lub pozyskiwaniu danych z internetu, co czyni go niezbędnym narzędziem w arsenale dewelopera.

## Jak to zrobić:

PHP obsługuje wyrażenia regularne za pośrednictwem biblioteki PCRE (Perl Compatible Regular Expressions), oferując bogaty zestaw funkcji. Oto jak ich używać:

### Dopasowywanie wzorca:

Aby sprawdzić, czy wzorzec istnieje w ciągu, użyj `preg_match()`. Ta funkcja zwraca 1, jeśli wzorzec został znaleziony w ciągu, i 0, jeśli nie.

```php
if (preg_match("/\bweb\b/i", "PHP to język skryptowy do stron internetowych")) {
    echo "Znaleziono dopasowanie.";
} else {
    echo "Nie znaleziono dopasowania.";
}
// Wynik: Znaleziono dopasowanie.
```

### Znajdowanie wszystkich dopasowań:

`preg_match_all()` jest używane, gdy potrzebujesz znaleźć wszystkie wystąpienia wzorca w ciągu.

```php
$text = "koty i psy";
$pattern = "/\b([a-z]+)\b/i";
preg_match_all($pattern, $text, $matches);
print_r($matches[0]);
// Wynik: Array ( [0] => koty [1] => i [2] => psy )
```

### Zastępowanie tekstu:

Do zastępowania tekstu, który pasuje do wyrażenia regularnego, używane jest `preg_replace()`. Jest niezwykle potężne do formatowania i porządkowania danych.

```php
$originalText = "15 kwietnia 2003";
$pattern = "/(\w+) (\d+), (\d+)/i";
$replacement = '${1}1,$3';
echo preg_replace($pattern, $replacement, $originalText);
// Wynik: 15 kwietnia1,2003
```

### Dzielenie ciągów:

Możesz podzielić ciąg na tablicę za pomocą `preg_split()`, określając wzorzec dla separatora.

```php
$text = "PHP jest, niezwykle popularnym, językiem skryptowym";
$parts = preg_split("/,\s*/", $text);
print_r($parts);
// Wynik: Array ( [0] => PHP jest [1] => niezwykle popularnym [2] => językiem skryptowym )
```

Ponadto, dla złożonych wzorców regex i zadań, frameworki i biblioteki takie jak komponent `Finder` w Symfony lub kolekcja pomocniczych funkcji w Laravelu mogą zapewnić bardziej wygodną warstwę abstrakcji. Jednak zrozumienie i wykorzystanie wbudowanych funkcji PCRE w PHP jest kluczowe do efektywnego przetwarzania tekstu i walidacji bezpośrednio w skryptach PHP.
