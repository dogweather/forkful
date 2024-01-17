---
title:                "Велика перша літера у рядку"
html_title:           "TypeScript: Велика перша літера у рядку"
simple_title:         "Велика перша літера у рядку"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/typescript/capitalizing-a-string.md"
---

{{< edit_this_page >}}

Що і чому? 

З несподіваного істочника, випливає необхідність написання коду, який може бути непередбачуваним для користувача. Це може викликати плутанину, особливо коли кожне слово по-своєму називається. І саме тут на допомогу приходить capitalizing у string. Це означає, що перше слово в рядку буде написано великими літерами, а всі інші - малими. Таким чином, рядок буде більш читабельним і легше розпізнаватися для програміста.

Як це зробити: 

```TypeScript 
const str = "цей рядок потрібно зробити capitalizing"; 
console.log(str.toUpperCase()); 
``` 
Вивід: 
Це рядок потрібно зробити capitalizing 

Глибовка дослідження: 

Capitalizing не є новим поняттям і використовувалося у багатьох стародавніх персідських і навіть римських текстах. У сучасному програмуванні існує кілька альтернативних способів capitalizing, таких як capitalize першої букви кожного слова, або capitalize кожної букви. У TypeScript, є вбудована функція, яка дозволяє застосувати capitalize до будь-якого рядка, дозволяючи програмістам зробити відсутність чіткого розділення між словами більш правильним.

Приклади: 

```TypeScript 
const str = "цей рядок потрібно зробити capitalize"; 
console.log(str.toLocaleLowerCase()); //трохи змінений рядок 
``` 
Вивід: 
цей рядок потрібно зробити capitalize 

##Дивіться також: 

- Документація TypeScript capitalize функції [https://www.typescriptlang.org/docs/handbook/utility-types.html#capitalizet](https://www.typescriptlang.org/docs/handbook/utility-types.html#capitalize)
- Історія розвитку capitalize в програмуванні [https://en.wikipedia.org/wiki/Capitalization](https://en.wikipedia.org/wiki/Capitalization)