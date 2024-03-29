---
changelog:
- 2024-02-01, gpt-4-0125-preview, translated from English
date: 2024-02-01 22:05:56.589325-07:00
description: "\u0420\u0435\u0433\u0443\u043B\u044F\u0440\u043D\u0456 \u0432\u0438\u0440\
  \u0430\u0437\u0438 (regex) \u2014 \u0446\u0435 \u0448\u0430\u0431\u043B\u043E\u043D\
  \u0438, \u0449\u043E \u0432\u0438\u043A\u043E\u0440\u0438\u0441\u0442\u043E\u0432\
  \u0443\u044E\u0442\u044C\u0441\u044F \u0434\u043B\u044F \u043F\u043E\u0448\u0443\
  \u043A\u0443 \u043A\u043E\u043C\u0431\u0456\u043D\u0430\u0446\u0456\u0439 \u0441\
  \u0438\u043C\u0432\u043E\u043B\u0456\u0432 \u0443 \u0440\u044F\u0434\u043A\u0430\
  \u0445. \u041F\u0440\u043E\u0433\u0440\u0430\u043C\u0456\u0441\u0442\u0438 \u0432\
  \u0438\u043A\u043E\u0440\u0438\u0441\u0442\u043E\u0432\u0443\u044E\u0442\u044C \u0457\
  \u0445 \u0434\u043B\u044F \u043F\u043E\u0448\u0443\u043A\u0443, \u0440\u0435\u0434\
  \u0430\u0433\u0443\u0432\u0430\u043D\u043D\u044F\u2026"
lastmod: '2024-03-13T22:44:48.492604-06:00'
model: gpt-4-0125-preview
summary: "\u0420\u0435\u0433\u0443\u043B\u044F\u0440\u043D\u0456 \u0432\u0438\u0440\
  \u0430\u0437\u0438 (regex) \u2014 \u0446\u0435 \u0448\u0430\u0431\u043B\u043E\u043D\
  \u0438, \u0449\u043E \u0432\u0438\u043A\u043E\u0440\u0438\u0441\u0442\u043E\u0432\
  \u0443\u044E\u0442\u044C\u0441\u044F \u0434\u043B\u044F \u043F\u043E\u0448\u0443\
  \u043A\u0443 \u043A\u043E\u043C\u0431\u0456\u043D\u0430\u0446\u0456\u0439 \u0441\
  \u0438\u043C\u0432\u043E\u043B\u0456\u0432 \u0443 \u0440\u044F\u0434\u043A\u0430\
  \u0445. \u041F\u0440\u043E\u0433\u0440\u0430\u043C\u0456\u0441\u0442\u0438 \u0432\
  \u0438\u043A\u043E\u0440\u0438\u0441\u0442\u043E\u0432\u0443\u044E\u0442\u044C \u0457\
  \u0445 \u0434\u043B\u044F \u043F\u043E\u0448\u0443\u043A\u0443, \u0440\u0435\u0434\
  \u0430\u0433\u0443\u0432\u0430\u043D\u043D\u044F\u2026"
title: "\u0412\u0438\u043A\u043E\u0440\u0438\u0441\u0442\u0430\u043D\u043D\u044F \u0440\
  \u0435\u0433\u0443\u043B\u044F\u0440\u043D\u0438\u0445 \u0432\u0438\u0440\u0430\u0437\
  \u0456\u0432"
---

{{< edit_this_page >}}

## Що і чому?

Регулярні вирази (regex) — це шаблони, що використовуються для пошуку комбінацій символів у рядках. Програмісти використовують їх для пошуку, редагування або маніпулювання текстом та даними, що робить їх незамінними для завдань зі збігами шаблонів та аналізу даних.

## Як:

Використання регулярних виразів у Google Apps Script є простим завдяки синтаксису на основі JavaScript. Ось як ви можете включити regex у свої скрипти для загальних завдань, таких як пошук і перевірка даних.

### Пошук у рядках

Припустимо, ви хочете знайти, чи містить рядок певний шаблон, наприклад, адресу електронної пошти. Ось простий приклад:

```javascript
function findEmailInText(text) {
  var emailPattern = /\b[A-Za-z0-9._%+-]+@[A-Za-z0-9.-]+\.[A-Z|a-z]{2,}\b/;
  var found = text.match(emailPattern);
  if (found) {
    Logger.log("Знайдено: " + found[0]);
  } else {
    Logger.log("Електронну пошту не знайдено.");
  }
}

// Приклад використання
findEmailInText("Зв'яжіться з нами за адресою info@example.com.");
```

### Перевірка даних

Регулярні вирази сяють у перевірці даних. Нижче наведена функція, що перевіряє вхідний рядок, щоб переконатися, чи він відповідає простій політиці паролів (принаймні одна велика буква, одна мала буква і мінімум 8 символів).

```javascript
function validatePassword(password) {
  var passwordPattern = /^(?=.*[a-z])(?=.*[A-Z]).{8,}$/;
  return passwordPattern.test(password);
}

// Приклад виведення
Logger.log(validatePassword("Str0ngPass")); // Виводиться: true
Logger.log(validatePassword("weak"));       // Виводиться: false
```

## Поглиблене вивчення

Регулярні вирази в Google Apps Script успадковані від JavaScript, що вперше було стандартизовано у специфікації мови ECMAScript у червні 1997 року. Незважаючи на потужність, регулярні вирази іноді можуть призводити до заплутаного та важко підтримуваного коду, особливо при надмірному використанні або застосуванні для завдань складного зіставлення шаблонів, які можливо більш ефективно вирішити іншими методами аналізу.

Наприклад, хоча ви можете використовувати regex для аналізу HTML або XML на швидку руку, робити це зазвичай не рекомендується через вкладені та складні структури цих документів. Натомість інструменти, спеціально призначені для аналізу таких структур, наприклад, DOM-парсери для HTML, є більш надійними та зрозумілими.

Більш того, розробникам Google Apps Script слід бути уважними до потенційних проблем з продуктивністю при використанні складних шаблонів regex у завданнях з масштабною маніпуляцією текстом, оскільки обробка regex може бути ресурсоємною. У таких випадках розбиття завдання на простіші підзавдання або використання вбудованих функцій маніпулювання рядками може запропонувати кращий баланс продуктивності та підтримуваності.
