---
title:                "Початок нового проєкту"
html_title:           "Elm: Початок нового проєкту"
simple_title:         "Початок нового проєкту"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Getting Started"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/typescript/starting-a-new-project.md"
---

{{< edit_this_page >}}

## Що і чому?

Створення нового проекту - це процес налаштування сценарію, засобів роботи та структури даних з нуля. Програмісти роблять це, щоб реалізувати нову ідею або розв’язати нову проблему, максимально ефективно використовуючи ресурси.

## Як це робити:

За допомогою npm (Node package manager) і TypeScript можна легко створити новий проект.

```TypeScript
// Спочатку встановіть TypeScript
npm install -g typescript

// Потім створіть новий каталог для вашого проекту
mkdir my-new-project && cd my-new-project

// Ініціалізуйте новий проект npm
npm init

// Встановіть TypeScript як залежність розробки
npm install --save-dev typescript

// Створіть новий файл конфігурації TypeScript
tsc --init
```

Ці команди створюють новий проект TypeScript, готовий до роботи.

## Пірнання вглиб:

1. **Історичний контекст**: TypeScript вперше було розроблено Microsoft у 2012 році як суперпосісти JavaScript. Його мета - добавити строгий синтаксис типів для підвищення продуктивності розробників.

2. **Альтернативи**: JavaScript, CoffeeScript, Dart - це лише кілька з багатьох альтернатив TypeScript. Вибір між ними залежить від ваших вимог до проекту та особистих уподобань.

3. **Деталі реалізації**: Коли ви створюєте TypeScript проект, ви маєте контроль над конфігурацією засобу зборки, таким як Webpack або Parcel, що дозволяє вам використовувати різні загрузчики та плагіни, надаючи вам гнучкість у налаштуванні процесу зборки проекту.

## Див. також:

1. [Офіційний сайт TypeScript](https://www.typescriptlang.org/)
2. [TypeScript у деталях на Medium](https://medium.com/tech-tajawal/typescript-why-should-one-use-it-a539faa92010)
4. [Документація npm](https://docs.npmjs.com/)
5. [Порівняння між TypeScript та JavaScript на Stack Overflow](https://stackoverflow.com/questions/12694530/what-is-typescript-and-why-would-i-use-it-in-place-of-javascript)