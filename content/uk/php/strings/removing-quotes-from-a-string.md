---
title:                "Видалення лапок зі строки"
aliases:
- /uk/php/removing-quotes-from-a-string.md
date:                  2024-01-26T03:42:10.379583-07:00
model:                 gpt-4-0125-preview
simple_title:         "Видалення лапок зі строки"

tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/php/removing-quotes-from-a-string.md"
---

{{< edit_this_page >}}

## Що і чому?
Видалення лапок із рядка в PHP означає видалення тих надокучливих подвійних (`"`) або одинарних (`'`) символів лапок, які можуть зіпсувати логіку вашого коду або запити до бази даних. Програмісти роблять це для очищення або санітації вхідних даних, забезпечуючи безпечне використання або зберігання рядків.

## Як:
Ось простий приклад за допомогою вбудованих функцій PHP:

```php
$quotedString = "'Hello,' she said, \"It's a fine day!\"";
$unquotedString = str_replace(array("'", "\""), '', $quotedString);
echo $unquotedString; // Виводить: Hello, she said, Its a fine day!
```

Просто, чи не так? Функція `str_replace()` бере масив символів, які потрібно видалити із рядка, включаючи як одиночні, так і подвійні лапки.

## Поглиблений огляд
У ранні дні PHP розробникам доводилося бути особливо обережними з лапками в рядках, особливо при вставці даних до бази даних. Неналежно оброблені лапки могли призвести до атак SQL ін'єкцій. Тоді була введена функція магічних лапок, яка автоматично екранувала вхідні дані. Вона була визнана застарілою та нарешті видалена, оскільки спонукала до поганих практик програмування та проблем з безпекою.

Тепер ми використовуємо функції, як-от `str_replace()`, або регулярні вирази з `preg_replace()` для більш складних шаблонів. Ось приклад з регулярним виразом:

```php
$quotedString = "'Hello,' she said, \"It's a fine day!\"";
$unquotedString = preg_replace('/[\'"]/', '', $quotedString);
echo $unquotedString;
```

Для даних JSON ви можете використовувати `json_encode()` з опціями, такими як `JSON_UNESCAPED_SLASHES | JSON_UNESCAPED_UNICODE`, щоб уникнути додаткових зворотних слешів у ваших лапках.

При реалізації враховуйте крайові випадки. Що, якщо ваш рядок призначений мати певні лапки, як діалог у розповіді або дюйми в мірках? Контекст має значення, отже, налаштуйте своє видалення лапок згідно з передбачуваним використанням даних.

## Див. також
- [PHP: str_replace](https://www.php.net/manual/en/function.str-replace.php)
- [PHP: preg_replace](https://www.php.net/manual/en/function.preg-replace.php)
- [PHP: json_encode](https://www.php.net/manual/en/function.json-encode.php)
- [OWASP: Профілактика SQL ін'єкцій](https://owasp.org/www-community/attacks/SQL_Injection)
