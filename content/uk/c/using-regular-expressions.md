---
title:                "Використання регулярних виразів"
html_title:           "C: Використання регулярних виразів"
simple_title:         "Використання регулярних виразів"
programming_language: "C"
category:             "C"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/c/using-regular-expressions.md"
---

{{< edit_this_page >}}

# Що і Зачем?

Використання регулярних виразів - це спосіб пошуку та обробки тексту за певними шаблонами. Це корисний інструмент для програмістів, оскільки дозволяє автоматизувати задачі, пов'язані з аналізом та зміною тексту.

# Як:

Мова програмування C має вбудовану підтримку для регулярних виразів за допомогою бібліотеки <regex.h>. Нижче наведено приклад коду для пошуку всіх слів "привіт" у рядку і виведення їх кількості:

```
#include <stdio.h>
#include <regex.h>

int main() {
    int count = 0;
    char str[] = "Привіт, світ! Привіт, Україно!";
    regex_t regex;
    regmatch_t matches[1];
    regcomp(&regex, "привіт", REG_EXTENDED | REG_ICASE);

    while(regexec(&regex, str, 1, matches, 0) == 0) {
        count++;
        str += matches[0].rm_eo;
    }

    printf("Кількість слів \"привіт\": %d", count);

    return 0;
}
```

Вивід: ```Кількість слів "привіт": 2```

# Поглиблене дослідження:

Поняття регулярних виразів з'явилося в 1950-х роках і зараз використовується в багатьох мовах програмування та текстових редакторах. Існують альтернативи, такі як бібліотека PCRE (Perl Compatible Regular Expressions), яка надає більш розширені можливості та регулярний вираз може використовуватися безпосередньо у вигляді рядка.

# Дивись також:

- [Регулярні вирази в мові програмування C](https://www.cprogramming.com/tutorial/regular-expressions-c.html)
- [Документація бібліотеки regex.h](https://www.gnu.org/software/libc/manual/html_node/Regular-Expressions.html)
- [Онлайн-версія для перевірки регулярних виразів](https://regex101.com/)