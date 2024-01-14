---
title:    "C: Написання тестів"
keywords: ["C"]
---

{{< edit_this_page >}}

## Чому

Написання тестів є важливою складовою частиною процесу програмування. Воно допомагає відловлювати та виправляти помилки ще до того, як програма буде випущена, тим самим заощаджуючи час та гроші. Тести також допомагають підтвердити та підтримувати правильну функціональність програмного забезпечення з часом.

## Як

Для написання тестів у мові програмування C потрібно використовувати бібліотеку Unity, яка надає різні функції для перевірки та здійснення асерцій. Нижче наведено приклад коду для функції обчислення факторіалу та виводу результату:

```C
int factorial(int num) {
    if (num == 0 || num == 1) {
        return 1;
    }
    else {
        return num * factorial(num - 1);
    }
}

printf("Факторіал числа 5: %d", factorial(5));

//output: Факторіал числа 5: 120
```

Цей приклад демонструє використання асерцій та перевірку правильності обчислення факторіалу.

## Глибока поглибінна аналітика

Написання тестів вимагає від програміста уваги до дрібниць та ретельності під час написання коду. Важливо перевіряти кожний можливий шлях виконання коду та включати різні тестові сценарії, щоб бути впевненим у правильності його роботи. Також важливо регулярно оновлювати написані тести у разі змін у програмі.

## Дивись також

- [Документація Unity для написання тестів у мові C](https://github.com/ThrowTheSwitch/Unity)
- [Стаття про переваги написання тестів у програмуванні (українською)](https://medium.com/@andriyburda/%D0%B2%D0%BB%D0%B0%D1%81%D1%82%D0%B8%D0%B2%D0%BE%D1%81%D1%82%D1%96-%D0%BD%D0%B0%D0%BF%D0%B8%D1%81%D0%B0%D0%BD%D0%BD%D1%8F-%D1%82%D0%B5%D1%81%D1%82%D1%96%D0%B2-%D0%BF%D1%80%D0%B8%D0%BC%D0%B5%D0%BD%D1%88%D0%B5%D0%BD%D0%BD%D1%8F-%D1%83-%D0%BF%D1%80%D0%BE%D0%B3%D1%80%D0%B0%D0%BC%D1%83%D0%B2%D0%B0%D0%BD%D0%BD%D1%96-8205cc328323)
- [Відео з оглядом основ написання тестів у мові C (українською)](https://www.youtube.com/watch?v=GwCmN3v0V1U)