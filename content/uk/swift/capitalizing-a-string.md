---
title:    "Swift: Великі літери для рядка"
keywords: ["Swift"]
---

{{< edit_this_page >}}

#Чому

У програмуванні є багато повсякденних завдань, одним з яких є робота з рядками тексту. Однією з корисних функцій, яка належить до роботи з рядками, є велика буква в початковому положенні. Це може бути корисно для забезпечення правильної вимови імен, прізвищ, або просто для естетичного вигляду в деяких випадках. У цій статті ми розглянемо, як можна легко здійснити цю операцію в мові програмування Swift.

#Як це зробити

Для початку, нам потрібно створити змінну з рядком тексту, який ми хочемо обробити:

```Swift
var name = "марина"
```

Для того, щоб здійснити великі букви в початковому положенні, ми можемо використати вбудовану функцію String `capitalized`, яка змінює першу букву рядка на велику:

```Swift
var capitalizedString = name.capitalized
```

Тепер, якщо ми виведемо змінну `capitalizedString`, ми побачимо, що перша буква стала великою:

```Swift
print(capitalizedString) // Марина
```

Якщо ми хочемо зберегти цю зміну у нашій оригінальній змінній `name`, ми можемо використати функцію `replaceSubrange` для заміни початкового рядка на змінений:

```Swift
name.replaceSubrange(name.startIndex...name.startIndex, with: capitalizedString)
```

Альтернативно, ми можемо використовувати цей метод простоїше, створюючи окрему змінну для збереження результату:

```Swift
name = name.capitalized
```

#Глибше в деталі

Функція `capitalized` фактично використовує інший вбудований метод `capitalized(with:)`, який приймає в параметри різні налаштування для форматування тексту. Наприклад, ми можемо вказати локацію, за якою ми хочемо виконати форматування, встановивши параметр `locale: Locale.current`. Це може бути корисно, якщо ми хочемо форматувати назви, що виходять за межі стандартної локалізації.

Також, в якості альтернативи `capitalized`, ми можемо використовувати метод `localizedCapitalized`, який буде форматувати рядок, враховуючи поточну мову системи.

#Дивіться також

1. [Документація Swift: String](https://docs.swift.org/swift-book/LanguageGuide/StringsAndCharacters.html)
2. [Стаття на тему: Контроль рядками в Swift](https://www.hackingwithswift.com/read/0/16/strings-and-integers)
3. [Інтер