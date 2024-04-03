---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:05:56.243136-07:00
description: 'Come fare: PHP supporta nativamente diverse funzioni per capitalizzare
  le stringhe, ognuna con uno scopo differente. Ecco come puoi utilizzarle: #.'
lastmod: '2024-03-13T22:44:43.500667-06:00'
model: gpt-4-0125-preview
summary: PHP supporta nativamente diverse funzioni per capitalizzare le stringhe,
  ognuna con uno scopo differente.
title: Capitalizzare una stringa
weight: 2
---

## Come fare:
PHP supporta nativamente diverse funzioni per capitalizzare le stringhe, ognuna con uno scopo differente. Ecco come puoi utilizzarle:

### Capitalizzare la prima lettera di una stringa:
```php
$string = "hello, world!";
$capitalizedString = ucfirst($string);
echo $capitalizedString; // Restituisce: Hello, world!
```

### Capitalizzare la prima lettera di ogni parola:
```php
$string = "hello, world!";
$capitalizedWords = ucwords($string);
echo $capitalizedWords; // Restituisce: Hello, World!
```

### Convertire l'intera stringa in maiuscolo:
```php
$string = "hello, world!";
$upperCaseString = strtoupper($string);
echo $upperCaseString; // Restituisce: HELLO, WORLD!
```

Per scenari che richiedono maggiore personalizzazione o soluzioni di terze parti, possono essere utilizzate librerie come `mbstring` (per stringhe multibyte), specialmente quando si tratta di internazionalizzazione dove i caratteri potrebbero estendersi oltre il set ASCII base.

### Utilizzare mbstring per capitalizzare stringhe UTF-8:
Assicurati di avere l'estensione `mbstring` abilitata nella tua configurazione PHP, poi:

```php
$string = "élégant";
$capitalizedString = mb_convert_case($string, MB_CASE_TITLE, "UTF-8");
echo $capitalizedString; // Restituisce: Élégant
```

Questo approccio aiuta a capitalizzare accuratamente le stringhe che includono caratteri non ASCII, aderendo alle sfumature di varie lingue.
