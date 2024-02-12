---
title:                "Преобразование строки в нижний регистр"
aliases: - /ru/typescript/converting-a-string-to-lower-case.md
date:                  2024-01-28T23:56:42.352629-07:00
model:                 gpt-4-0125-preview
simple_title:         "Преобразование строки в нижний регистр"

tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ru/typescript/converting-a-string-to-lower-case.md"
changelog:
  - 2024-01-28, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Что и Почему?
Преобразование строки в нижний регистр означает, что каждый символ в строке становится маленькой буквой. Программисты делают это для обеспечения единообразия, особенно для сравнений без учета регистра, например, при проверке пользовательского ввода на соответствие списку команд или сохраненным данным.

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
