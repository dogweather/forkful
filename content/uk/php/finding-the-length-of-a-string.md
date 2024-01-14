---
title:    "PHP: Знаходження довжини рядка."
keywords: ["PHP"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/uk/php/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## Чому

Існує багато ситуацій, коли нам потрібно знати довжину рядка у своїй PHP програмі. Наприклад, при валідації форм, отриманні короткого опису для публікації або при обробці даних з бази даних. Тому знання як отримувати довжину рядка є важливою навичкою для будь-якого PHP розробника.

## Як

```PHP
<?php
$string = "Привіт, світ!";
// Використовуємо функцію mb_strlen() для отримання довжини рядка з урахуванням кирилиці
$length = mb_strlen($string);
echo "Довжина рядка \"$string\" складає $length символів.";
?>
```

Виведе:

```
Довжина рядка "Привіт, світ!" складає 12 символів.
```

Просто подібно із функцією `strlen()`, яка не враховує множинність символів в UTF-8 кодуванні, функція `mb_strlen()` дозволяє коректно отримувати довжину кирилічних рядків. У випадку, якщо встановлення `mbstring` не активоване на вашому сервері, ви можете скористатися функцією `iconv_strlen()`.

## Deep Dive

В PHP, рядки можна представити у двох форматах - однобайтовому та многобайтовому. Однобайтові рядки складаються з 8-бітних символів, тоді як многобайтові можуть складатися з більшості цих символів. Враховуючи розширення PHP, `mb_strlen()` та `iconv_strlen()` необхідно користуватися у випадку, коли ви працюєте з многобайтовими рядками.

Існують також інші функції для отримання довжини рядка, такі як `mb_strwidth()` та `mb_substr()`, які дозволяють вирішити різні проблеми з многобайтовими рядками.

## Дивіться також

- [PHP Manual on mb_strlen()](https://www.php.net/manual/en/function.mb-strlen.php)
- [PHP Manual on strlen()](https://www.php.net/manual/en/function.strlen.php)
- [PHP Manual on iconv_strlen()](https://www.php.net/manual/en/function.iconv-strlen.php)
- [PHP Manual on mb_strwidth()](https://www.php.net/manual/en/function.mb-strwidth.php)
- [PHP Manual on mb_substr()](https://www.php.net/manual/en/function.mb-substr.php)