---
title:                "Конвертування дати в рядок"
html_title:           "C++: Конвертування дати в рядок"
simple_title:         "Конвертування дати в рядок"
programming_language: "C++"
category:             "C++"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/cpp/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## Чому

Конвертування дати у рядок часто використовується в програмуванні для виведення дати у зрозумілому для користувача форматі, а також для обробки та збереження дати у різноманітних форматах.

## Як це зробити

```C++
#include <iostream>
#include <string>
#include <ctime>

using namespace std;

int main()
{
	// отримання поточної дати та часу
	time_t currentTime = time(0);
	tm* localTime = localtime(&currentTime);

	// форматування дати у рядок
	char buffer[80];
	strftime(buffer, 80, "%d.%m.%Y", localTime);

	// виведення результату
	string dateString(buffer);
	cout << "Сьогоднішня дата у форматі dd.mm.yyyy: " << dateString << endl;

	return 0;
}
```

Вивід:

```
Сьогоднішня дата у форматі dd.mm.yyyy: 23.04.2021
```

## Поглиблене дослідження

Конвертування дати у рядок відбувається за допомогою функції `strftime()` з бібліотеки `<ctime>`. Ця функція приймає три аргументи: рядок, у який буде записана дата, розмір цього рядка та структуру `tm`, яка містить інформацію про дату та час. Другим аргументом є формат, за яким буде виведено дату. За допомогою спеціальних символів можна задати різні формати дати та часу, такі як `%d` для дня, `%m` для місяця та `%Y` для року. Повний список цих символів можна знайти в документації до функції `strftime()`.

## Дивіться також

- [Офіційна документація з функції strftime()](https://en.cppreference.com/w/cpp/chrono/c/strftime)
- [Стрічки у C++](https://www.geeksforgeeks.org/char-vs-stdstring-vs-char-c/)
- [Структури у C++](https://www.tutorialspoint.com/cplusplus/cpp_data_structures.htm)