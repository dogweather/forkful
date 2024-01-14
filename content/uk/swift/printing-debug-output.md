---
title:    "Swift: Вивід налагодження друку"
keywords: ["Swift"]
---

{{< edit_this_page >}}

## Чому

Вивід дебаг-інформації є важливою частиною програмування, яка дозволяє вам відслідковувати помилки та налагоджувати свій код. Це можливо завдяки виведенню значень змінних та повідомлень про стан вашого коду.

## Як

Існує кілька способів виводу дебаг-інформації в Swift. Один з найпростіших - використання функції `print()`. Наприклад:

```Swift
let name = "Іван"
print("Привіт, мене звуть \(name)!")
```

Цей код виведе у консоль повідомлення "Привіт, мене звуть Іван!".

Також ви можете виводити дебаг-інформацію за допомогою функції `debugPrint()`, яка виводить не тільки значення змінних, але і їхні типи даних. Наприклад:

```Swift
let age = 25
debugPrint("Мені \(age) років.")
```

Результатом буде "Мені 25 років." зі зміненним курсором після значення 25. Це дозволяє більш детально вивчати дані та їхню структуру.

## Глибоке погруження

Крім виведення повідомлень та значень змінних, ви також можете використовувати дебаг-інформацію для вивчення роботи вашої програми в реальному часі. Наприклад, ви можете виводити інформацію про поточний стан програми в певній точці, використовуючи умовний оператор `if`:

```Swift
let status = "Підключено"
if status == "Підключено" {
    print("Пристрій підключений до мережі.")
} else {
    print("Пристрій не підключений до мережі.")
}
```

Цей код перевіряє, чи є значення `status` рівним "Підключено", і залежно від результата виводить відповідне повідомлення.

## Дивіться також

- [Apple Developer Documentation: Debugging with Xcode](https://developer.apple.com/documentation/xcode/debugging_with_xcode)
- [Ray Wenderlich: Debugging in Swift: Quick Guide for Developers](https://www.raywenderlich.com/5134-debugging-in-swift-quick-guide-for-developers)