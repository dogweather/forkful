---
title:                "PHP: Видалення символів, відповідних заданому шаблону"
simple_title:         "Видалення символів, відповідних заданому шаблону"
programming_language: "PHP"
category:             "PHP"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/php/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

Markdown:
## Чому
Якщо ви програміст PHP, ви, можливо, зіткнулися з ситуацією, коли необхідно видалити певні символи з рядка. Це може бути корисно для очищення даних, наприклад, з введення користувача.

## Як видалити символи, що відповідають шаблону
Існує кілька способів видалення символів, що відповідають певному шаблону в PHP. Один із способів - використовувати функцію ```preg_replace()```:

```
$string = "Hello, Ukrainian readers!";
$pattern = '/[a-z]/';
$replaced_string = preg_replace($pattern, '', $string);
echo $replaced_string;

// Вивід: HU
```
У цьому прикладі ми використовуємо шаблон ```/[a-z]/```, щоб видалити всі малі літери з рядка. Ви можете змінити шаблон, щоб видаляти будь-який інший символ або навіть декілька символів за раз.

Ще один спосіб - використовувати функцію ```str_replace()```, яка заміняє всі збіги з шаблоном певним рядком:

```
$string = "Hello, Ukrainian readers!";
$replaced_string = str_replace('e', '', $string);
echo $replaced_string;

// Вивід: Hllo, Ukraiian rads!
```

У цьому прикладі ми видалили всі літери 'e' з рядка. Якщо бажаєте видалити декілька символів, просто додайте їх до першого аргументу функції ```str_replace()```.

Іншим способом є використання функції ```substr()```, яка повертає певну частину рядка з заданої позиції:

```
$string = "Hello, Ukrainian readers!";
$replaced_string = substr($string, 7);
echo $replaced_string;

// Вивід: Ukrainain readers!
```

У цьому прикладі ми видалили перші 7 символів з рядка, залишивши тільки слова "Ukrainian readers!".

Простий спосіб видалити певні символи з рядка - використовувати цикл та умовні оператори:

```
$string = "Hello, Ukrainian readers!";
$length = strlen($string);
$replaced_string = "";
for ($i = 0; $i < $length; $i++) {
    if ($string[$i] != 'l' && $string[$i] != 'e') {
        $replaced_string .= $string[$i];
    }
}
echo $replaced_string;

// Вивід: Ho, Ukainian raders!
```

У цьому прикладі ми перевіряємо кожен символ за допомогою умовного оператора і додаємо його до нового рядка, якщо він не відповідає символам, які ми хочемо видалити.

## Deep Dive
У PHP є багато функцій для роботи з рядками, тому можливо ви знайдете ще біль