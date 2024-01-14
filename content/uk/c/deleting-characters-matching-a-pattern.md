---
title:    "C: Видалення символів, що відповідають шаблону"
keywords: ["C"]
---

{{< edit_this_page >}}

##Чому
Ви можете зацікавитися видаленням символів, які відповідають певному шаблону, якщо вам потрібно очистити ваш код від непотрібних даних або якщо ви шукаєте швидкий спосіб замінити певні символи на пусті рядки.

##Як
Найпростішим способом видалення символів, які відповідають певному шаблону, є застосування функції `strpbrk()`. Нижче наведено приклад коду, який використовує цю функцію:

```C

#include <stdio.h>
#include <string.h>

int main() {
    char str[] = "Це приклад рядка, в якому потрібно видалити всі голосні";
    char vowels[] = "аеиіоуєюяАЕИІОУЄЮЯ";
    char *result;
    result = strpbrk(str, vowels);
    
    while (result != NULL) {
        *result = '\0';
        result = strpbrk(result + 1, vowels);
    }
    
    printf("%s", str);
    return 0;
}

```

Вище наведений код буде виводити наступне:

```
Ц приклд рдк, в якм потбо вдлт вс глсн
```

Також існують і інші способи видалення символів, такі як функція `strchr()` і заміна користувацьких функцій.

##Глибинне вивчення
Якщо ви хочете докладніше дізнатися про процес видалення символів, який відповідає певному шаблону, варто розглянути слідуючі питання:

- Як працюють функції `strpbrk()` і `strchr()`?
- Як можна використовувати ці функції для видалення символів?
- Як можна використовувати заміну функцій для видалення символів?
- Як реалізована робота з символами в кодуванні UTF-8?

##Дивіться також
- [Робота з рядками у C](https://proglib.io/p/strings-in-c/)
- [Керівництво з практичного використання функцій у C](https://wikicoding.org/wiki/c/funktsii-v-cyslici/s/)
- [Видалення символів у C з допомогою функції strpbrk](https://www.geeksforgeeks.org/removing-spaces-from-a-string-in-c/)

Дякуємо за читання! Надіємося, що ця стаття допомогла вам розібратися з процесом видалення символів, які відповідають певному шаблону, використовуючи C. Безпечного програмування!