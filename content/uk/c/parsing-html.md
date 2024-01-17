---
title:                "Розбір html"
html_title:           "C: Розбір html"
simple_title:         "Розбір html"
programming_language: "C"
category:             "C"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/c/parsing-html.md"
---

{{< edit_this_page >}}

## Що & чому?
Розбір HTML - це процес отримання інформації з HTML-коду, що використовується для форматування веб-сторінок. Це необхідно для забезпечення коректного відображення веб-сторінок та отримання необхідних даних з них.

## Як це виконати:
Цей код демонструє, як отримати заголовок сторінки з HTML-коду за допомогою функції `strstr()`:

```C
#include <string.h>
#include <stdio.h>

char* get_title(char* html) {
    char* title = strstr(html, "<title>") + 7;
    char* title_end = strstr(title, "</title>");
    
    int length = title_end - title;
    char* result = malloc((length + 1) * sizeof(char));
    strncpy(result, title, length);
    result[length] = '\0';
    
    return result;
}

int main() {
    char* html = "<html><head><title>Заголовок сторінки</title></head></html>";
    printf("Заголовок сторінки: %s\n", get_title(html));
    
    return 0;
}
```

Виконаємо цей код та отримаємо наступний результат:
```
Заголовок сторінки: Заголовок сторінки
```

## Поглиблене вивчення:
Розбір HTML був необхідний на початку веб-розробки, коли створювалися прості HTML-сторінки з допомогою тегів. Проте, з появою більш складних веб-додатків та технологій, таких як JavaScript та CSS, розбір HTML став потужнішим та має багато альтернативних методів виконання.

Реалізація розбору HTML може бути більш простою за допомогою сторонніх бібліотек, таких як libxml та libcurl. Також існує можливість використання XPath для отримання конкретних даних з HTML-коду.

## Дивись також:
- [libxml](http://www.xmlsoft.org/)
- [libcurl](https://curl.haxx.se/libcurl/)
- [XPath](https://www.w3.org/TR/xpath/)