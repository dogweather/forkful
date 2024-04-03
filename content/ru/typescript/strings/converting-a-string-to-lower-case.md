---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 23:56:42.352629-07:00
description: "\u041A\u0430\u043A \u044D\u0442\u043E \u0441\u0434\u0435\u043B\u0430\
  \u0442\u044C: \u0412 TypeScript \u043F\u0440\u0435\u043E\u0431\u0440\u0430\u0437\
  \u043E\u0432\u0430\u043D\u0438\u0435 \u0441\u0442\u0440\u043E\u043A\u0438 \u0432\
  \ \u043D\u0438\u0436\u043D\u0438\u0439 \u0440\u0435\u0433\u0438\u0441\u0442\u0440\
  \ - \u043F\u0440\u043E\u0449\u0435 \u043F\u0440\u043E\u0441\u0442\u043E\u0433\u043E\
  . \u041F\u0440\u043E\u0441\u0442\u043E \u0432\u044B\u0437\u043E\u0432\u0438\u0442\
  \u0435 \u043C\u0435\u0442\u043E\u0434 `.toLowerCase()` \u0434\u043B\u044F \u0432\
  \u0430\u0448\u0435\u0439 \u0441\u0442\u0440\u043E\u043A\u0438. \u0412\u043E\u0442\
  \ \u043A\u0430\u043A."
lastmod: '2024-03-13T22:44:44.565569-06:00'
model: gpt-4-0125-preview
summary: "\u0412 TypeScript \u043F\u0440\u0435\u043E\u0431\u0440\u0430\u0437\u043E\
  \u0432\u0430\u043D\u0438\u0435 \u0441\u0442\u0440\u043E\u043A\u0438 \u0432 \u043D\
  \u0438\u0436\u043D\u0438\u0439 \u0440\u0435\u0433\u0438\u0441\u0442\u0440 - \u043F\
  \u0440\u043E\u0449\u0435 \u043F\u0440\u043E\u0441\u0442\u043E\u0433\u043E."
title: "\u041F\u0440\u0435\u043E\u0431\u0440\u0430\u0437\u043E\u0432\u0430\u043D\u0438\
  \u0435 \u0441\u0442\u0440\u043E\u043A\u0438 \u0432 \u043D\u0438\u0436\u043D\u0438\
  \u0439 \u0440\u0435\u0433\u0438\u0441\u0442\u0440"
weight: 4
---

## Как это сделать:
В TypeScript преобразование строки в нижний регистр - проще простого. Просто вызовите метод `.toLowerCase()` для вашей строки. Вот как:

```typescript
let myString: string = "HeLLo, WorLD!";
let lowerCaseString: string = myString.toLowerCase();
console.log(lowerCaseString); // Вывод: "hello, world!"
```

Просто, не так ли?

## Подробнее
В старые времена обработка текста не всегда была последовательной, и кодировка символов могла быть диким западом. Теперь, благодаря Unicode и стандартизированным методам, регистры однородны на протяжении разных языков. По сравнению с `.toLowerCase()`, старомодный подход (такой как манипуляции с ASCII) кажется каменным веком. Альтернативы (например, `.toLocaleLowerCase()`) учитывают локально-специфические правила для правильного кейсинга, что может быть полезно. Под капотом, метод `.toLowerCase()` в JavaScript (и, соответственно, в TypeScript) проходит через каждый символ и, если это большая буква, преобразует её в ее эквивалент в нижнем регистре, основываясь на сопоставлениях Unicode.

## Смотрите также
Для более продвинутой работы со строками и чтобы разнообразить вашу игру обработки текста, взгляните на:

- Документация MDN по `.toLowerCase()`: [MDN toLowerCase](https://developer.mozilla.org/ru/docs/Web/JavaScript/Reference/Global_Objects/String/toLowerCase)
- Официальная документация TypeScript: [TypeScriptlang.org](https://www.typescriptlang.org/docs/)
- Чтобы лучше понять трансформации, специфичные для локали: [MDN toLocaleLowerCase](https://developer.mozilla.org/ru/docs/Web/JavaScript/Reference/Global_Objects/String/toLocaleLowerCase)
- Для глубокого изучения стандартов Unicode: [Unicode Case Mapping](https://www.unicode.org/reports/tr21/tr21-5.html)
