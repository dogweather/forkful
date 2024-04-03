---
changelog:
- 2024-01-29, gpt-4-0125-preview, translated from English
date: 2024-01-29 00:03:40.198298-07:00
description: "\u041D\u0430\u0447\u0430\u043B\u043E \u043D\u043E\u0432\u043E\u0433\u043E\
  \ \u043F\u0440\u043E\u0435\u043A\u0442\u0430 \u043D\u0430 TypeScript \u0437\u0430\
  \u043A\u043B\u044E\u0447\u0430\u0435\u0442\u0441\u044F \u0432 \u0441\u043E\u0437\
  \u0434\u0430\u043D\u0438\u0438 \u043D\u0430\u0434\u0435\u0436\u043D\u043E\u0433\u043E\
  \ \u0444\u0443\u043D\u0434\u0430\u043C\u0435\u043D\u0442\u0430 \u0434\u043B\u044F\
  \ \u043D\u0430\u043F\u0438\u0441\u0430\u043D\u0438\u044F \u043A\u043E\u0434\u0430\
  . \u041F\u0440\u043E\u0433\u0440\u0430\u043C\u043C\u0438\u0441\u0442\u044B \u0437\
  \u0430\u043F\u0443\u0441\u043A\u0430\u044E\u0442 \u043D\u043E\u0432\u044B\u0435\
  \ \u043F\u0440\u043E\u0435\u043A\u0442\u044B, \u0447\u0442\u043E\u0431\u044B \u043F\
  \u0440\u0435\u0432\u0440\u0430\u0442\u0438\u0442\u044C\u2026"
lastmod: '2024-03-13T22:44:44.590428-06:00'
model: gpt-4-0125-preview
summary: "\u041D\u0430\u0447\u0430\u043B\u043E \u043D\u043E\u0432\u043E\u0433\u043E\
  \ \u043F\u0440\u043E\u0435\u043A\u0442\u0430 \u043D\u0430 TypeScript \u0437\u0430\
  \u043A\u043B\u044E\u0447\u0430\u0435\u0442\u0441\u044F \u0432 \u0441\u043E\u0437\
  \u0434\u0430\u043D\u0438\u0438 \u043D\u0430\u0434\u0435\u0436\u043D\u043E\u0433\u043E\
  \ \u0444\u0443\u043D\u0434\u0430\u043C\u0435\u043D\u0442\u0430 \u0434\u043B\u044F\
  \ \u043D\u0430\u043F\u0438\u0441\u0430\u043D\u0438\u044F \u043A\u043E\u0434\u0430\
  ."
title: "\u041D\u0430\u0447\u0430\u043B\u043E \u043D\u043E\u0432\u043E\u0433\u043E\
  \ \u043F\u0440\u043E\u0435\u043A\u0442\u0430"
weight: 1
---

## Как это сделать:
```TypeScript
// Шаг 1: Установите TypeScript глобально (если не установлен)
npm install -g typescript

// Шаг 2: Создайте новую директорию для вашего проекта
mkdir my-new-project
cd my-new-project

// Шаг 3: Инициализируйте новый node проект
npm init -y

// Шаг 4: Установите TypeScript в ваш проект
npm install typescript --save-dev

// Шаг 5: Инициализируйте проект TypeScript, чтобы создать tsconfig.json
tsc --init

// Пример вывода tsconfig.json (с некоторыми пропущенными полями для краткости)
{
  "compilerOptions": {
    "target": "es5",
    "module": "commonjs",
    "strict": true,
    ...
  }
}

// Шаг 6: Создайте простой файл TypeScript 'hello.ts'
echo 'console.log("Привет, TypeScript!");' > hello.ts

// Шаг 7: Компилируйте файл TypeScript и запустите его
tsc hello.ts
node hello.js

// Пример вывода
Привет, TypeScript!
```

## Погружение в тему
TypeScript, надмножество JavaScript, был разработан Microsoft и впервые выпущен в октябре 2012 года. Он добавляет в JavaScript статические типы, что может помочь обнаружить ошибки до выполнения программы и поддерживать возможности среды разработки, такие как навигация по коду и рефакторинг.

Хотя в вышеуказанной процедуре используется npm (Node Package Manager), существуют и другие способы управления проектами на TypeScript, такие как Yarn или pnpm. Альтернативы инициализации проекта TypeScript включают создание проекта с помощью стартового набора или клонирование шаблона из репозиториев, таких как GitHub.

`tsconfig.json` является ключевым; он указывает, как компилятор TypeScript (tsc) преобразует ваш TypeScript код в JavaScript. Настройка параметров компилятора позволяет вам нацеливаться на разные версии ECMAScript, системы модулей и многое другое, настраивая под нужды вашего проекта.

## Смотрите также
- Официальная документация TypeScript: [https://www.typescriptlang.org/docs/](https://www.typescriptlang.org/docs/)
- Репозиторий TypeScript на GitHub: [https://github.com/microsoft/TypeScript](https://github.com/microsoft/TypeScript)
- Глубокое погружение в TypeScript: [https://basarat.gitbook.io/typescript/](https://basarat.gitbook.io/typescript/)
- Прекрасный TypeScript: [https://github.com/dzharii/awesome-typescript](https://github.com/dzharii/awesome-typescript)
