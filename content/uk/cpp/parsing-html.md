---
title:                "Аналізування html"
html_title:           "C++: Аналізування html"
simple_title:         "Аналізування html"
programming_language: "C++"
category:             "C++"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/cpp/parsing-html.md"
---

{{< edit_this_page >}}

## Чому

В даній статті, ми дізнаємося, як парсити HTML у програмі на мові C++. Це дуже важливий процес при роботі з веб-додатками або при отриманні даних з Інтернету. Парсинг HTML дозволяє нам зчитувати та обробляти дані зі сторінок веб-сайтів.

## Як це зробити

Для того, щоб парсити HTML у програмі на мові C++, нам знадобиться використати зовнішні бібліотеки. Найбільш популярні з них - це "libcurl", "pugixml" та "Boost" бібліотеки. Давайте розглянемо приклад парсингу HTML за допомогою бібліотеки "pugixml" у стилі "SAX" (Simple API for XML):

 ```C++
#include <iostream>
#include <pugixml.hpp>

void parse_html(const char* buffer) {
    pugi::xml_document doc;
    pugi::xml_parse_result result = doc.load(buffer);

    // Перевірка результату парсингу
    if (!result) {
        std::cout << "Помилка парсингу: " << result.description() << "\n";
    }

    // Отримання кореневого вузла документу
    pugi::xml_node root = doc.document_element();
   
    // Пошук і обробка тегу "title"
    pugi::xml_node title = root.child("head").child("title");
 
    if (title) {
        std::cout << "Назва веб-сторінки: " << title.child_value() << "\n";
    }
    
    // Пошук і обробка всіх тегів "a" на сторінці
    for (pugi::xml_node link = root.child("body").child("a"); link; link = link.next_sibling("a")) {
        std::cout << "Посилання: " << link.attribute("href").value() << "\n";
    }
}

int main() {
    const char* buffer = "<html><head><title>Мій перший веб-сайт</title></head></html>";
    parse_html(buffer);
    return 0;
}
```

Вихідний код демонструє створення "html" документу за допомогою рядка тексту і подальшого його парсингу за допомогою бібліотеки "pugixml". У результаті парсингу ми отримаємо назву веб-сторінки та всі посилання з тегами "a" на сторінці.

## Глибше погляд

Парсинг HTML за допомогою бібліотеки "pugixml" є більш швидким, ніж за допомогою інших бібліотек, таких як "Boost" або "libxml2". Бібліотека "pugixml" підтримує технологію "SAX", що дозволяє обробку документа у вигляді потоку, що забезпечує низький рівень використання пам'яті та збереження часу.

## Дивись також

- [Офіційна документація "pugixml" бібліотеки](https://pugixml.org/docs/manual.html)
- [Реалізаці