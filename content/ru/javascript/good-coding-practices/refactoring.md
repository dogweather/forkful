---
changelog:
- 2024-01-29, gpt-4-0125-preview, translated from English
date: 2024-01-29 00:02:13.445829-07:00
description: "\u041A\u0430\u043A: \u0414\u0430\u0432\u0430\u0439\u0442\u0435 \u0440\
  \u0430\u0441\u0441\u043C\u043E\u0442\u0440\u0438\u043C \u043F\u0440\u043E\u0441\u0442\
  \u043E\u0439 \u043F\u0440\u0438\u043C\u0435\u0440, \u0433\u0434\u0435 \u0440\u0435\
  \u0444\u0430\u043A\u0442\u043E\u0440\u0438\u043D\u0433 \u043C\u043E\u0436\u0435\u0442\
  \ \u0441\u0434\u0435\u043B\u0430\u0442\u044C \u0432\u0430\u0448 \u043A\u043E\u0434\
  \ \u0431\u043E\u043B\u0435\u0435 \u043A\u0440\u0430\u0442\u043A\u0438\u043C \u0438\
  \ \u0447\u0438\u0442\u0430\u0435\u043C\u044B\u043C. \u0417\u0434\u0435\u0441\u044C\
  \ \u043C\u044B \u0440\u0435\u0444\u0430\u043A\u0442\u043E\u0440\u0438\u043C \u0444\
  \u0443\u043D\u043A\u0446\u0438\u044E, \u043A\u043E\u0442\u043E\u0440\u0430\u044F\
  \ \u0432\u044B\u0447\u0438\u0441\u043B\u044F\u0435\u0442\u2026"
lastmod: '2024-03-13T22:44:45.775975-06:00'
model: gpt-4-0125-preview
summary: "\u0414\u0430\u0432\u0430\u0439\u0442\u0435 \u0440\u0430\u0441\u0441\u043C\
  \u043E\u0442\u0440\u0438\u043C \u043F\u0440\u043E\u0441\u0442\u043E\u0439 \u043F\
  \u0440\u0438\u043C\u0435\u0440, \u0433\u0434\u0435 \u0440\u0435\u0444\u0430\u043A\
  \u0442\u043E\u0440\u0438\u043D\u0433 \u043C\u043E\u0436\u0435\u0442 \u0441\u0434\
  \u0435\u043B\u0430\u0442\u044C \u0432\u0430\u0448 \u043A\u043E\u0434 \u0431\u043E\
  \u043B\u0435\u0435 \u043A\u0440\u0430\u0442\u043A\u0438\u043C \u0438 \u0447\u0438\
  \u0442\u0430\u0435\u043C\u044B\u043C."
title: "\u0420\u0435\u0444\u0430\u043A\u0442\u043E\u0440\u0438\u043D\u0433"
weight: 19
---

## Как:
Давайте рассмотрим простой пример, где рефакторинг может сделать ваш код более кратким и читаемым. Здесь мы рефакторим функцию, которая вычисляет сумму массива чисел.

До:
```javascript
function calculateSum(arr) {
  let sum = 0;
  for (let i = 0; i < arr.length; i++) {
    sum += arr[i];
  }
  return sum;
}

console.log(calculateSum([1, 2, 3, 4])); // Вывод: 10
```

После:
```javascript
function calculateSum(arr) {
  return arr.reduce((sum, num) => sum + num, 0);
}

console.log(calculateSum([1, 2, 3, 4])); // Вывод: 10
```

Видите, как метод `reduce` сокращает размер функции, сохраняя при этом функциональность? Вот что такое рефакторинг.

## Глубокое погружение
Рефакторинг не стал формальной практикой до публикации книги Мартина Фаулера "Refactoring: Improving the Design of Existing Code" в 1999 году. Эта книга, наряду с ростом популярности гибкой разработки программного обеспечения, помогла перевести рефакторинг в мейнстрим.

Описание рефакторинга как аспекта разработки программного обеспечения похоже на объяснение, почему вы убираетесь в мастерской: вы делаете это, чтобы в следующий раз, когда вам нужно будет что-то починить (в данном случае, код), вы тратили меньше времени на борьбу с беспорядком и больше на решение самой проблемы.

Когда мы говорим о альтернативах рефакторингу, мы входим в более широкое обсуждение стратегий поддержки программного обеспечения. Например, можно выбрать полную перезапись кода, но это часто бывает дороже и рискованнее. Рефакторинг постепенно приносит постоянные преимущества без риска потерять все из-за внезапного пересмотра.

Рефакторингу помогло развитие интегрированных сред разработки (IDE) и инструментов, таких как JSHint, ESLint и Prettier в экосистеме JavaScript, которые автоматизируют проверку качества кода и выявляют возможности для рефакторинга.

Всё сводится к чистому, выразительному и поддерживаемому коду. Сложные алгоритмы, оптимизации структур данных или даже архитектурные изменения, такие как переход от процедурного к функциональному стилю программирования, могут быть частью процесса рефакторинга.

Рефакторинг необходимо проводить осторожно; важно иметь надежный набор тестов, чтобы убедиться, что ваши изменения не изменили поведение программного обеспечения неожиданным образом — еще одна причина, почему разработка, управляемая тестированием (TDD), хорошо сочетается с рефакторингом, так как она по умолчанию предоставляет эту защиту.

## См. также
- Книга Мартина Фаулера по рефакторингу: [Refactoring - Improving the Design of Existing Code](https://martinfowler.com/books/refactoring.html)
- Фреймворки для тестирования JavaScript (для проверки, что рефакторинг не нарушит функциональности):
  - Jest: [Jest - Delightful JavaScript Testing](https://jestjs.io/)
  - Mocha: [Mocha - забавный, простой, гибкий фреймворк для тестирования JavaScript](https://mochajs.org/)

- Инструменты для проверки качества кода и поддержки рефакторинга:
  - ESLint: [ESLint - Настраиваемый линтер JavaScript](https://eslint.org/)
  - Prettier: [Prettier - Упрямый форматировщик кода](https://prettier.io/)
