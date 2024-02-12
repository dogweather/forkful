---
title:                "Capitalizing a string"
aliases:
- en/php/capitalizing-a-string.md
date:                  2024-02-03T19:02:32.487209-07:00
model:                 gpt-4-0125-preview
simple_title:         "Capitalizing a string"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/php/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## What & Why?
Capitalizing a string involves modifying the first character of given text to uppercase, ensuring sentences, titles, or proper names start correctly in a dataset. Programmers often execute string capitalization for data normalization, improving readability or ensuring consistency in user input or textual data processing.

## How to:
PHP natively supports various functions to capitalize strings, each serving a different purpose. Here's how you can use them:

### Capitalizing the first letter of a string:

```php
$string = "hello, world!";
$capitalizedString = ucfirst($string);
echo $capitalizedString; // Outputs: Hello, world!
```

### Capitalizing the first letter of each word:

```php
$string = "hello, world!";
$capitalizedWords = ucwords($string);
echo $capitalizedWords; // Outputs: Hello, World!
```

### Converting the entire string to uppercase:

```php
$string = "hello, world!";
$upperCaseString = strtoupper($string);
echo $upperCaseString; // Outputs: HELLO, WORLD!
```

For scenarios requiring more customization or third-party solutions, libraries like `mbstring` (for multibyte strings) can be utilized, especially when dealing with internationalization where characters might extend beyond the basic ASCII set.

### Using mbstring to capitalize UTF-8 strings:

Ensure you have the `mbstring` extension enabled in your PHP configuration, then:

```php
$string = "élégant";
$capitalizedString = mb_convert_case($string, MB_CASE_TITLE, "UTF-8");
echo $capitalizedString; // Outputs: Élégant
```

This approach helps to accurately capitalize strings that include non-ASCII characters, adhering to the nuances of various languages.
