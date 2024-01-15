---
title:                "Генерація випадкових чисел"
html_title:           "Fish Shell: Генерація випадкових чисел"
simple_title:         "Генерація випадкових чисел"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/fish-shell/generating-random-numbers.md"
---

{{< edit_this_page >}}

## Чому?

Створення випадкових чисел є важливою частиною багатьох програм та скриптів. Воно дозволяє генерувати унікальні значення для різноманітних ситуацій, таких як генерація паролів, тестування програм або створення унікальних ідентифікаторів.

## Як це зробити?

```Fish Shell``` має вбудовану команду ```random```, яка дозволяє генерувати випадкові числа зазначеної довжини за допомогою різних методів генерації.

Наприклад, для генерації випадкового числа з діапазону від 1 до 10, ви можете використовувати таку команду:

```
set random_number (random --minimum 1 --maximum 10)
echo $random_number
```

Ця команда створить випадкове число та присвоїть його змінній ```$random_number```, а потім виведе його на екран.

Додатковим параметром цієї команди є ```--hex```, який дозволяє генерувати випадкове шістнадцяткове число.

## Глибше в деталі

За замовчуванням, команда ```random``` використовує метод мережі Fibonacci для генерації випадкових чисел. Проте, ви можете вибрати інший метод генерації за допомогою параметра ```--method```.

Наприклад, щоб використовувати метод Лемера в замість методу Fibonacci, ви можете виконати таку команду:

```
set random_number (random --method lehmer)
```

Для більшої точності та криптографічної стійкості, ви можете використовувати параметр ```--secure```, який використовує міцний криптографічний генератор випадкових чисел.

## Подивіться також

- [Fish Shell документація по команді random](https://fishshell.com/docs/current/cmds/random.html)
- [Стаття про генерацію випадкових чисел в Bash Shell](https://idratherbewriting.com/learnapidoc/docapis_documentingapisviasamples.html#generatingrandomboolean)
- [Стаття про використання команди ```random``` в іграх в Fish Shell](https://medium.com/@olegfish/russian-randomnum-spruceup-your-random-in-fish-shell-9f939d569dac?source=friends_link&sk=c245d8e09b20ee7e1bca4da3d26bc75e)