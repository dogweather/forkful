---
title:                "Капіталізація рядка"
html_title:           "PHP: Капіталізація рядка"
simple_title:         "Капіталізація рядка"
programming_language: "PHP"
category:             "PHP"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/php/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## Що і чому?
Капіталізація рядків - це процес перетворення першої літери в кожному слові рядка на велику літеру. Це часто використовується програмістами для поліпшення читабельності тексту та дотримання стандартів написання коду.

## Як?
```PHP
<?php
$string = "привіт світ";
echo ucfirst($string); // Виводить "Привіт світ"
```

## Глибоке погруження
Капіталізація рядків вперше з'явилася в 16 столітті в англійській мові. У програмуванні, є кілька альтернативних методів для капіталізації рядків, таких як використання функцій `strtoupper` та `ucwords`. Однак, `ucfirst` функція є більш універсальною, оскільки капіталізує лише першу букву рядка з урахуванням культурних нюансів.

## Дивіться також
Детальніше про функцію `ucfirst` можна дізнатися у [документації PHP](https://www.php.net/manual/en/function.ucfirst.php). Для порівняння, можна ознайомитися з іншими функціями для капіталізації рядків, такими як [`strtoupper`](https://www.php.net/manual/en/function.strtoupper.php) та [`ucwords`](https://www.php.net/manual/en/function.ucwords.php).