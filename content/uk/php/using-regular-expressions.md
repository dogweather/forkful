---
title:                "Використання регулярних виразів"
date:                  2024-01-19
html_title:           "Bash: Використання регулярних виразів"
simple_title:         "Використання регулярних виразів"

category:             "PHP"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/php/using-regular-expressions.md"
---

{{< edit_this_page >}}

## What & Why? (Що і Чому?)
Регулярні вирази (regex) - це могутній інструмент для пошуку та маніпуляції текстом. Програмісти використовують їх через універсальність і здатність швидко обробляти складні шаблони.

## How to (Як це робити):
PHP має вбудовані функції для роботи з regex, такі як `preg_match`, `preg_replace` тощо. Нижче наведено декілька прикладів:

```php
<?php
// Перевірка на відповідність email
$email = "hello@example.com";
if (preg_match("/^[a-zA-Z0-9._%-]+@[a-zA-Z0-9.-]+\.[a-zA-Z]{2,4}$/", $email)) {
  echo "Email валідний.";
} else {
  echo "Email не валідний.";
}

// Заміна тексту
$text = "Привіт, це номер +38(099)123-45-67.";
$newText = preg_replace("/\+38\(\d{3}\)\d{3}-\d{2}-\d{2}/", "прихований номер", $text);
echo $newText; // Виводить: "Привіт, це прихований номер."
?>
```

## Deep Dive (Занурення у деталі):
- **Історичний контекст**: Регулярні вирази беруть свій початок ще з 1950-х, але дійшли до нас у вдосконаленій формі завдяки роботі Кені Томпсона у 1960-х.
- **Альтернативи**: Для деяких задач можна використовувати функції `strpos`, `str_replace` та ін. вони простіші, але менш потужні.
- **Деталі реалізації**: PHP використовує PCRE (Perl Compatible Regular Expressions) бібліотеку, яка забезпечує широкі можливості для роботи з regex.

## See Also (Додатково):
- [PHP: PCRE Functions](https://www.php.net/manual/en/ref.pcre.php) - Офіційна документація PHP по функціям PCRE.
- [Regex101](https://regex101.com/) - Інтерактивний редактор регулярних виразів для тестування і вивчення.
- [Regular-Expressions.info](https://www.regular-expressions.info/) - Поглиблені ресурси і зразки для вивчення regex.
