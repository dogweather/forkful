---
title:                "Пошук та заміна тексту"
html_title:           "C++: Пошук та заміна тексту"
simple_title:         "Пошук та заміна тексту"
programming_language: "PHP"
category:             "PHP"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/php/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## Що і чому?

Пошук та заміна тексту - це процес, який допомагає змінити визначений текст на інший всередині коду. Програмісти роблять це щодня, щоб виправдати зміни в бізнес-правилах або оновити код, який став застарілим.

## Як це зробити:

Ось основний приклад, який ілюструє, як пошукати та замінити текст у PHP.

```PHP
<?php
    // Вхідний текст
    $text = "Вітаю на AnfisaNews!"; 
   // Заміна 'AnfisaNews' на 'AnfisaUpdate'
    echo str_replace("AnfisaNews", "AnfisaUpdate", $text); 
?>
```

Виходом буде: 
```PHP 
"Вітаю на AnfisaUpdate!" 
```
Таким чином, 'AnfisaNews' замінилося на 'AnfisaUpdate'.

## Докладніше:

Історично, PHP мала функцію `ereg_replace()`, але вона була вилучена у PHP 7.0.0 на користь `preg_replace()` та `str_replace()`.  Що означає, що ми не можемо використовувати `ereg_replace()` в новіших версіях PHP.

Альтернативою `str_replace()` є `preg_replace()`. 'preg_' в цьому випадку означає Perl-сумісний регулярний вираз, й цей варіант є більш потужним, але і складнішим використанні.

Реалізація `str_replace()` може здійснюватися послідовно або через хеш-таблицю, в залежності від того, яким є вхід. 

## Дивись також:

- PHP Manual Entry on 'str_replace': https://www.php.net/manual/en/function.str-replace.php
- PHP Manual Entry on 'preg_replace': https://www.php.net/manual/en/function.preg-replace.php
- Stack Overflow Post on the Differences between 'str_replace' and 'preg_replace': https://stackoverflow.com/questions/10752815/difference-between-str-replace-and-preg-replace