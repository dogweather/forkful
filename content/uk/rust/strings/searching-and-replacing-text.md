---
date: 2024-01-20 17:58:58.978476-07:00
description: "\u041F\u043E\u0448\u0443\u043A \u0442\u0430 \u0437\u0430\u043C\u0456\
  \u043D\u0430 \u0442\u0435\u043A\u0441\u0442\u0443 \u2014 \u0446\u0435 \u043F\u0440\
  \u043E\u0446\u0435\u0441 \u0456\u0434\u0435\u043D\u0442\u0438\u0444\u0456\u043A\u0430\
  \u0446\u0456\u0457 \u043F\u0435\u0432\u043D\u0438\u0445 \u0437\u0440\u0430\u0437\
  \u043A\u0456\u0432 \u0442\u0435\u043A\u0441\u0442\u0443 \u0442\u0430 \u0457\u0445\
  \ \u0437\u0430\u043C\u0456\u043D\u0438 \u043D\u0430 \u0456\u043D\u0448\u0456. \u041F\
  \u0440\u043E\u0433\u0440\u0430\u043C\u0456\u0441\u0442\u0438 \u0432\u0438\u043A\u043E\
  \u0440\u0438\u0441\u0442\u043E\u0432\u0443\u044E\u0442\u044C \u0446\u0435 \u0434\
  \u043B\u044F \u0440\u0435\u0434\u0430\u0433\u0443\u0432\u0430\u043D\u043D\u044F\
  \ \u043A\u043E\u0434\u0443, \u043E\u0431\u0440\u043E\u0431\u043A\u0438\u2026"
lastmod: '2024-03-13T22:44:48.912156-06:00'
model: gpt-4-1106-preview
summary: "\u041F\u043E\u0448\u0443\u043A \u0442\u0430 \u0437\u0430\u043C\u0456\u043D\
  \u0430 \u0442\u0435\u043A\u0441\u0442\u0443 \u2014 \u0446\u0435 \u043F\u0440\u043E\
  \u0446\u0435\u0441 \u0456\u0434\u0435\u043D\u0442\u0438\u0444\u0456\u043A\u0430\u0446\
  \u0456\u0457 \u043F\u0435\u0432\u043D\u0438\u0445 \u0437\u0440\u0430\u0437\u043A\
  \u0456\u0432 \u0442\u0435\u043A\u0441\u0442\u0443 \u0442\u0430 \u0457\u0445 \u0437\
  \u0430\u043C\u0456\u043D\u0438 \u043D\u0430 \u0456\u043D\u0448\u0456. \u041F\u0440\
  \u043E\u0433\u0440\u0430\u043C\u0456\u0441\u0442\u0438 \u0432\u0438\u043A\u043E\u0440\
  \u0438\u0441\u0442\u043E\u0432\u0443\u044E\u0442\u044C \u0446\u0435 \u0434\u043B\
  \u044F \u0440\u0435\u0434\u0430\u0433\u0443\u0432\u0430\u043D\u043D\u044F \u043A\
  \u043E\u0434\u0443, \u043E\u0431\u0440\u043E\u0431\u043A\u0438\u2026"
title: "\u041F\u043E\u0448\u0443\u043A \u0442\u0430 \u0437\u0430\u043C\u0456\u043D\
  \u0430 \u0442\u0435\u043A\u0441\u0442\u0443"
---

{{< edit_this_page >}}

## Що і чому?
Пошук та заміна тексту — це процес ідентифікації певних зразків тексту та їх заміни на інші. Програмісти використовують це для редагування коду, обробки даних або автоматизації задач.

## Як це зробити:
```Rust
fn main() {
    let text = "Привіт, світ!";
    let updated_text = text.replace("світ", "Світ");
    println!("{}", updated_text);
}

// Виведення: Привіт, Світ!
```

## Поглиблений розгляд:
Пошук та заміна тексту має давню історію у програмуванні, починаючи із ранніх утиліт командного рядка, як `sed`. Руст надає різноманітні способи здійснення цієї задачі, включаючи метод `replace()` для рядків, що є простим і прямолінійним. Для складніших задач можна використовувати регулярні вирази з крейтом `regex`. Регулярні вирази дозволяють визначати складні шаблони для пошуку та заміни, але вимагають більше часу на навчання та менш читабельні.

## Дивіться також:
- Руст Book про рядки: https://doc.rust-lang.org/book/ch08-02-strings.html
- Документація `regex` крейта: https://docs.rs/regex/
- `sed` історія та приклади: https://www.gnu.org/software/sed/manual/sed.html
