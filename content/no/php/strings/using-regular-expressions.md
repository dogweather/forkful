---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:17:40.724162-07:00
description: "Hvordan: PHP st\xF8tter regul\xE6re uttrykk gjennom PCRE-biblioteket\
  \ (Perl Compatible Regular Expressions), som tilbyr et rikt sett med funksjoner.\
  \ Her er\u2026"
lastmod: '2024-03-13T22:44:40.875587-06:00'
model: gpt-4-0125-preview
summary: "PHP st\xF8tter regul\xE6re uttrykk gjennom PCRE-biblioteket (Perl Compatible\
  \ Regular Expressions), som tilbyr et rikt sett med funksjoner."
title: "Bruke regul\xE6re uttrykk"
weight: 11
---

## Hvordan:
PHP støtter regulære uttrykk gjennom PCRE-biblioteket (Perl Compatible Regular Expressions), som tilbyr et rikt sett med funksjoner. Her er hvordan du bruker dem:

### Matche et mønster:
For å sjekke om et mønster finnes innenfor en streng, bruk `preg_match()`. Denne funksjonen returnerer 1 hvis mønsteret ble funnet i strengen, og 0 hvis ikke.

```php
if (preg_match("/\bweb\b/i", "PHP er et web scriptingspråk")) {
    echo "En match ble funnet.";
} else {
    echo "Ingen match ble funnet.";
}
// Output: En match ble funnet.
```

### Finne alle treff:
`preg_match_all()` brukes når du trenger å finne alle forekomster av et mønster innenfor en streng.

```php
$text = "katter og hunder";
$pattern = "/\b([a-z]+)\b/i";
preg_match_all($pattern, $text, $matches);
print_r($matches[0]);
// Output: Array ( [0] => katter [1] => og [2] => hunder )
```

### Erstatte tekst:
For å erstatte tekst som matcher et regulært uttrykk, brukes `preg_replace()`. Det er utrolig kraftfullt for formatering og opprydding av data.

```php
$originalText = "15. april 2003";
$pattern = "/(\w+) (\d+), (\d+)/i";
$replacement = '${1}1,$3';
echo preg_replace($pattern, $replacement, $originalText);
// Output: april1,2003
```

### Dele strenger:
Du kan dele en streng inn i et array ved å bruke `preg_split()`, og spesifisere et mønster for skilletegnet.

```php
$text = "PHP er, et ekstremt populært, scriptingspråk";
$parts = preg_split("/,\s*/", $text);
print_r($parts);
// Output: Array ( [0] => PHP er [1] => et ekstremt populært [2] => scriptingspråk )
```

Videre, for komplekse regex-mønstre og oppgaver, kan rammeverk og biblioteker som Symfony sitt `Finder`-komponent eller Laravel sin samling av hjelpefunksjoner tilby et mer praktisk abstraksjonslag. Likevel er forståelsen og bruk av PHPs innebygde PCRE-funksjoner avgjørende for effektiv tekstbehandling og validering direkte inne i PHP-skript.
