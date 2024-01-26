---
title:                "Використання регулярних виразів"
html_title:           "Bash: Використання регулярних виразів"
simple_title:         "Використання регулярних виразів"
programming_language: "C++"
category:             "C++"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/cpp/using-regular-expressions.md"
---

{{< edit_this_page >}}

## Що це та Навіщо?
Регулярні вирази – це шаблони, що використовуються для пошуку й маніпуляції текстом. Програмісти вдаються до них, щоб здійснювати складний пошук, валідацію, заміну чи витягування даних, зекономлюючи час та рядки коду.

## Як це робити:
В C++ регулярні вирази доступні через бібліотеку `<regex>`. Дивіться приклади нижче:

```C++
#include <iostream>
#include <regex>
#include <string>

int main() {
    std::string text = "Привіт, я програміст з України!";
    std::regex word_regex(R"(програміст)");
    
    // Перевірка на відповідність слова
    bool match = std::regex_search(text, word_regex);
    std::cout << "Знайдено? " << (match ? "Так" : "Ні") << std::endl;

    // Знаходження й виведення всіх відповідностей
    std::sregex_iterator words_begin = std::sregex_iterator(text.begin(), text.end(), word_regex);
    std::sregex_iterator words_end = std::sregex_iterator();
    for (std::sregex_iterator i = words_begin; i != words_end; ++i) {
        std::smatch match = *i;
        std::string match_str = match.str();
        std::cout << "Знайдено: " << match_str << std::endl;
    }
    return 0;
}
```

Очікуваний вивід:
```
Знайдено? Так
Знайдено: програміст
```

## Поглиблено:
Регулярні вирази мають свої коріння в теорії автоматів і математиці. Спочатку використані в Unix-подібних системах, вони широко впроваджені в багатьох мовах програмування. Альтернативою їм можуть бути різні парсери або власні реалізації алгоритмів пошуку в тексті, але регулярні вирази найчастіше є найзручнішим та найшвидшим рішенням. Їхнє використання через `<regex>` в C++ – це рішення стандарту, що включає класи та функції для роботи з регулярними виразами.

## Додатково:
- [cppreference.com: std::regex](https://en.cppreference.com/w/cpp/regex)
- [Regular-Expressions.info: Tutorial](https://www.regular-expressions.info/tutorial.html)

Звертайтеся до цих ресурсів, щоб поглибити свої знання з регулярних виразів.
