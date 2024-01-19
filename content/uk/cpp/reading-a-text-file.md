---
title:                "Читання текстового файлу"
html_title:           "Arduino: Читання текстового файлу"
simple_title:         "Читання текстового файлу"
programming_language: "C++"
category:             "C++"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/cpp/reading-a-text-file.md"
---

{{< edit_this_page >}}

## Що та чому?

Читання текстового файлу - це процес витягування даних з такого файлу у програму. Це зазвичай роблять, коли програмі треба працювати з зовнішньою інформацією, яка зберігається в текстовому форматі.

## Як це робити:

Ось приклад коду для читання текстового файлу у C++.

```C++
#include <fstream>
#include <iostream>
#include <string>

int main() {
	std::ifstream file("example.txt");
	std::string str;

	while (std::getline(file, str)) {
		std::cout << str << std::endl;
	}

	return 0;
}
```

Цей код зчитує кожну строку текстового файлу 'example.txt' та виводить її на консоль.

## Поглиблено:
Читання текстових файлів було важливою частиною програмування з самого початку, коли інформація зберігалася на перфокартах.
Сучасні альтернативи включають читання з реляційних баз даних, NoSQL баз даних або дані в облаках. Однак текстові файли все ще широко використовуються через їх простоту і універсальність.
Функція 'std::getline' читає файл лінію за лінією. Це працює швидше, ніж читання по одному символу, але потрібно врахувати, що великі файли можуть зайняти багато пам'яті, тому їх слід читати послідовно.

## Дивитися також:

- C++ Documentation on `std::getline`: https://en.cppreference.com/w/cpp/string/basic_string/getline
- C++ Documentation on Input/Output with files: https://www.cplusplus.com/doc/tutorial/files/
- StackOverflow Thread on "Read large files line by line in C++": https://stackoverflow.com/questions/7868936/read-file-line-by-line-using-ifstream-in-c
- Efficient File IO in C++: https://www.bfilipek.com/2018/07/file-reading-cpp.html