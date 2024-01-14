---
title:                "C++: Написання тестів"
programming_language: "C++"
category:             "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/cpp/writing-tests.md"
---

{{< edit_this_page >}}

## Чому 

Тестування є важливою частиною розробки програмного забезпечення, оскільки допомагає перевірити правильну роботу програми та виявити помилки. Написання тестів допомагає забезпечити якість коду та зменшити кількість помилок у продукті. 

## Як 

Наприклад, маємо функцію додавання двох чисел:
```C++
int add(int a, int b) {
  return a + b;
}
```

Напишемо для неї тест, використовуючи бібліотеку Google Test:
```C++
TEST(AddTest, HandlesPositiveNumbers) {
  EXPECT_EQ(add(2, 3), 5);
}

TEST(AddTest, HandlesNegativeNumbers) {
  EXPECT_EQ(add(-2, -3), -5);
}
```

Виконання цих тестів підтвердить, що функція працює правильно та повертає очікувані значення. 

## Глибоке дослідження 

Написання тестів також допомагає забезпечити повну покриття коду, тобто перевіряється всі можливі вхідні дані та поведінка програми. Також використання тестів сприяє гнучкості та зручності при зміні функцій чи додаванні нових можливостей до програми. 

## Дивіться також 

- [Google Test документація](https://github.com/google/googletest/blob/master/googletest/docs/Primer.md)
- [Вступ до тестування програмного забезпечення](https://www.tutorialspoint.com/software_testing/software_testing_basics.htm)
- [Основні принципи тестування програмного забезпечення](https://www.geeksforgeeks.org/software-engineering-principles/)