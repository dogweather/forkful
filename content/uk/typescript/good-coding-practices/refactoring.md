---
date: 2024-01-26 03:36:56.782618-07:00
description: "\u0420\u0435\u0444\u0430\u043A\u0442\u043E\u0440\u0438\u043D\u0433 -\
  \ \u0446\u0435 \u043F\u0440\u043E\u0446\u0435\u0441 \u0440\u0435\u0441\u0442\u0440\
  \u0443\u043A\u0442\u0443\u0440\u0438\u0437\u0430\u0446\u0456\u0457 \u0456\u0441\u043D\
  \u0443\u044E\u0447\u043E\u0433\u043E \u043A\u043E\u043C\u043F'\u044E\u0442\u0435\
  \u0440\u043D\u043E\u0433\u043E \u043A\u043E\u0434\u0443 \u0431\u0435\u0437 \u0437\
  \u043C\u0456\u043D\u0438 \u0439\u043E\u0433\u043E \u0437\u043E\u0432\u043D\u0456\
  \u0448\u043D\u044C\u043E\u0457 \u043F\u043E\u0432\u0435\u0434\u0456\u043D\u043A\u0438\
  . \u041F\u0440\u043E\u0433\u0440\u0430\u043C\u0456\u0441\u0442\u0438 \u0440\u043E\
  \u0431\u043B\u044F\u0442\u044C \u0446\u0435 \u0434\u043B\u044F \u0442\u043E\u0433\
  \u043E, \u0449\u043E\u0431 \u043A\u043E\u0434 \u0441\u0442\u0430\u0432\u2026"
lastmod: '2024-03-13T22:44:48.880855-06:00'
model: gpt-4-0125-preview
summary: "\u0420\u0435\u0444\u0430\u043A\u0442\u043E\u0440\u0438\u043D\u0433 - \u0446\
  \u0435 \u043F\u0440\u043E\u0446\u0435\u0441 \u0440\u0435\u0441\u0442\u0440\u0443\
  \u043A\u0442\u0443\u0440\u0438\u0437\u0430\u0446\u0456\u0457 \u0456\u0441\u043D\u0443\
  \u044E\u0447\u043E\u0433\u043E \u043A\u043E\u043C\u043F'\u044E\u0442\u0435\u0440\
  \u043D\u043E\u0433\u043E \u043A\u043E\u0434\u0443 \u0431\u0435\u0437 \u0437\u043C\
  \u0456\u043D\u0438 \u0439\u043E\u0433\u043E \u0437\u043E\u0432\u043D\u0456\u0448\
  \u043D\u044C\u043E\u0457 \u043F\u043E\u0432\u0435\u0434\u0456\u043D\u043A\u0438\
  . \u041F\u0440\u043E\u0433\u0440\u0430\u043C\u0456\u0441\u0442\u0438 \u0440\u043E\
  \u0431\u043B\u044F\u0442\u044C \u0446\u0435 \u0434\u043B\u044F \u0442\u043E\u0433\
  \u043E, \u0449\u043E\u0431 \u043A\u043E\u0434 \u0441\u0442\u0430\u0432\u2026"
title: "\u0420\u0435\u0444\u0430\u043A\u0442\u043E\u0440\u0438\u043D\u0433"
---

{{< edit_this_page >}}

## Що і чому?
Рефакторинг - це процес реструктуризації існуючого комп'ютерного коду без зміни його зовнішньої поведінки. Програмісти роблять це для того, щоб код став чистішим, більш піддатливим до обслуговування, та щоб зменшити його складність, що робить його легшим для розуміння новачками.

## Як це робити:
Розглянемо функцію TypeScript, якій вже кращі часи - вона трохи заплутана і потребує ніжної уваги та догляду:

```typescript
function userInfo(data: any): string {
    return "User Info: " + data.name + ", " + data.age + ", " + data.email + ";" ;
}
```
Після рефакторингу, вона може виглядати так:

```typescript
interface User {
    name: string;
    age: number;
    email: string;
}

function formatUserInfo(user: User): string {
    return `User Info: ${user.name}, ${user.age}, ${user.email};`;
}
```

Другий приклад більш надійний, використовує систему типів TypeScript за допомогою `interface` для уникнення потенційних помилок під час виконання та покращення читабельності.

## Поглиблений огляд
Рефакторинг - це не сучасне поняття; він еволюціонував разом з програмуванням, ставши більш формалізованим після випуску книги Мартіна Фаулера "Refactoring: Improving the Design of Existing Code" у 1999 році. Він має вирішальне значення в Agile-середовищі розробки, сприяючи адаптивним змінам у коді. Деякі альтернативи ручному рефакторингу включають автоматизовані інструменти, такі як TSLint або власний сервер мови TypeScript, які можуть пропонувати або навіть виконувати певні завдання по рефакторингу за вас. Деталі реалізації зазвичай включають виявлення "запахів коду", таких як дубльований код, довгі методи або великі класи, та застосування шаблонів для їх виправлення - наприклад, екстракції методів, переміщення до більш підходящих класів або використання спрощених конструкцій. Ці шаблони ключові для розуміння як і чому рефакторингу.

## Дивіться також
- [Книга "Refactoring: Improving the Design of Existing Code" Мартіна Фаулера](https://martinfowler.com/books/refactoring.html)
- [TSLint для статичного аналізу коду](https://palantir.github.io/tslint/)
- [Розуміння запахів коду](https://refactoring.guru/refactoring/smells)
