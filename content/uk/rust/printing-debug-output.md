---
title:    "Rust: Виведення відладкового виводу"
keywords: ["Rust"]
---

{{< edit_this_page >}}

## Чому

Просування коду і подій у програмуванні часто є необхідністю для зрозуміння того, що відбувається в програмі. Виведення відладочних повідомлень дозволяє нам слідкувати за значенням змінних, контролювати виконання програми та виявляти помилки.

## Як

Для виведення відладочних повідомлень у Rust, зазвичай використовується макрос ```println!```. Цей макрос дозволяє вивести текст або значення змінної у вигляді рядка. Ось приклад коду:

```Rust
let name = "Bob";
println!("Hello, my name is {}", name);
```

А вихідним повідомленням буде:

```shell
Hello, my name is Bob
```

Макрос ```println!``` також має вбудовані форматувальні можливості, які дозволяють виводити значення змінних з різним типом даних. Наприклад:

```Rust
let number = 42;
println!("The answer to the ultimate question is {}", number);
```

Буде виведено:

```shell
The answer to the ultimate question is 42
```

## Глибокий занурення

Макрос ```println!``` є корисним, але іноді для виведення складних типів даних чи більшої кількості інформації потрібне щось більше. В таких випадках можна використати структуру ```Debug```, яка дозволяє налаштовувати виведення задля відображення більш детального та інформативного повідомлення. Для цього необхідно реалізувати для структури вбудований трейт ```Debug```. Ось приклад коду:

```Rust
#[derive(Debug)]
struct Student {
    name: String,
    age: u8,
    grade: u8,
}

let student = Student {
    name: "Anna".to_string(),
    age: 23,
    grade: 12,
};
println!("{:?}", student);
```

Отримаємо таке повідомлення:

```shell
Student { name: "Anna", age: 23, grade: 12 }
```

## Дивись також

- [Документація String Formatting в Rust](https://doc.rust-lang.org/std/fmt/)
- [Розділ про відлагну дебаггінг у курсі "Rust by Example"](https://doc.rust-lang.org/rust-by-example/hello/print/print_debug.html)
- [Стаття про дебаггінг у Rust на сайті Medium](https://medium.com/@cowboyd/how-to-debug-rust-with-printing-things-to-standard-error-b5914106b80b)