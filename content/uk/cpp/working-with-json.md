---
title:                "Робота з json"
html_title:           "C++: Робота з json"
simple_title:         "Робота з json"
programming_language: "C++"
category:             "C++"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/cpp/working-with-json.md"
---

{{< edit_this_page >}}

# Що і для чого?

Робота з JSON - це процес обробки та збереження даних у форматі JSON (JavaScript Object Notation). Це модернізована та більш зручна версія формату даних .js, що використовується в JavaScript. Програмісти використовують JSON для зберігання та передачі структурованих даних, таких як веб-сервіси та мобільні додатки.

# Як: 

Нижче наведені два приклади коду на C++, які показують, як працювати з JSON в програмуванні.

```C++
// Приклад 1: Зчитування та парсинг JSON даних з веб-сервера

#include <iostream>
#include <curl/curl.h> // бібліотека для роботи з мережею

// callback-функція для обробки отриманих даних
size_t WriteCallback(char* buf, size_t size, size_t nmemb, void* up) {
    // buf - буфер, в який записуються дані
    // size - розмір кожного елементу буферу
    // nmemb - кількість елементів в буфері
    // up - позначення даних що передаються у цій функції
    for (int c = 0; c < size * nmemb; c++) {
        std::cout << buf[c];
    }
    return size * nmemb; // повертаємо кількість оброблених байтів
}

// виконуємо запит до веб-сервера
void GetDataFromServer(const char* url) {
    CURL* curl; // об'єкт CURL
    curl = curl_easy_init(); // ініціалізуємо бібліотеку

    // встановлюємо URL, з якого нам потрібно зчитати дані
    curl_easy_setopt(curl, CURLOPT_URL, url);
    // вказуємо функцію, яка виконується для обробки отриманих даних
    curl_easy_setopt(curl, CURLOPT_WRITEFUNCTION, &WriteCallback);
    // запускаємо запит
    curl_easy_perform(curl);
    curl_easy_cleanup(curl); // завершуємо роботу з об'єктом CURL
}

int main() {
    // використовуємо функцію з') для отримання даних з веб-сервера
    GetDataFromServer("https://jsonplaceholder.typicode.com/posts/1");
    return 0;
}

// Виведена інформація:
// {"userId":1,"id":1,"title":"sunt aut facere repellat provident occaecati excepturi optio reprehenderit","body":"quia et suscipit\nsuscipit recusandae consequuntur expedita"}
```

```C++
// Приклад 2: Створення та запис JSON даних

#include <iostream>
#include <string>
#include "json.hpp" // бібліотека для роботи з JSON

// використання псевдоніма для коротшої назви бібліотеки
using json = nlohmann::json;

int main() {
    // створюємо об'єкт JSON
    json jsonData = {
        {"name", "John"},
        {"age", 30},
        {"hobbies", {"football", "reading"}}
    };

    // конвертуємо наш об'єкт у рядок з форматом JSON
    std::string jsonStr = jsonData.dump();

    // зберігаємо цей рядок у файл
    std::ofstream file("data.json");
    file << jsonStr;

    // виводимо інформацію про успіх запису у файл
    if (file.is_open()) {
        std::cout << "Data successfully saved to file!" << std::endl;
    } else {
        std::cout << "Error while saving data to file." << std::endl;
    }

    file.close(); // закриваємо файл

    return 0;
}

// Записані дані у файл:
// {"name":"John","age":30,"hobbies":["football","reading"]}
```

# Глибока зануреність:

JSON був створений з метою полегшення обміну даними між різними платформами та програмами. Він став більш популярним після випуску основних браузерів, що підтримують JavaScript, тому що це дозволило створювати динамічні веб-сайти із використанням JSON формату даних.

Сьогодні існує багато альтернатив JSON, такі як XML і YAML, але у багатьох випадках JSON залишається популярнішим та простішим у використанні, особливо під час роботи з JavaScript та веб-сервісами.

JSON підтримує 6 типів даних: рядки, числа, булеві значення, null, масиви та об'єкти. Він також підтриму