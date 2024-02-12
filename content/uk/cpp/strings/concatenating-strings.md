---
title:                "Об'єднання рядків"
aliases: - /uk/cpp/concatenating-strings.md
date:                  2024-01-20T17:34:16.296497-07:00
model:                 gpt-4-1106-preview
simple_title:         "Об'єднання рядків"

tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/cpp/concatenating-strings.md"
---

{{< edit_this_page >}}

## Що це таке та навіщо?

Конкатенація - це процес об'єднання двох або більше рядків у один. Програмісти використовують її, щоб зліпити текст, формувати повідомлення або створювати дані для зберігання чи відправки.

## Як це зробити:

```C++
#include <iostream>
#include <string>

int main() {
    std::string firstName = "Василь";
    std::string lastName = "Петренко";
    
    // Використання оператора '+'
    std::string fullName = firstName + " " + lastName;
    std::cout << fullName << std::endl; // Василь Петренко

    // Використання метода append()
    std::string greeting = "Привіт, ";
    greeting.append(fullName);
    std::cout << greeting << std::endl; // Привіт, Василь Петренко

    return 0;
}
```

## Поглиблено:

В C++ конкатенацію рядків можливо здійснити декількома шляхами. Починаючи з C++98, в стандартній бібліотеці з'явився клас `std::string`, який значно спростив роботу з текстом. До цього програмісти частіше використовували C-стильні рядки - масиви символів. 

Альтернативними способами конкатенації є використання операторів `+=` або `append()`. Вони можуть бути корисними, коли під час формування рядка потрібно додати інші рядки чи символи.

З погляду виконання, конкатенація рядків може бути витратною операцією, оскільки іноді вимагає виділення нової пам'яті та копіювання рядків. Сучасний C++ вводить рухомі семантики та переміщення рядків (move semantics), що може поліпшити продуктивність при конкатенації великих рядків.

## Ще Інфо:

- [C++ string reference](https://en.cppreference.com/w/cpp/string/basic_string)
- [C++ strings guide на cppreference](https://en.cppreference.com/w/cpp/string)
