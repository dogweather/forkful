---
title:                "텍스트 검색 및 교체"
html_title:           "Elixir: 텍스트 검색 및 교체"
simple_title:         "텍스트 검색 및 교체"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/typescript/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

# TypeScript와 함께 텍스트 검색 및 교체하기

## 무엇이며 왜 할까요?

텍스트 검색 및 교체는 문자열 내에서 특정 패턴 혹은 문자를 찾아 다른 문자열로 교체하는 기능입니다. 변화하는 요구사항을 빠르게 수용하며 코드를 유지보수하는데 아주 중요한 기능입니다.

## 어떻게 할까요?

JavaScript와 마찬가지로 TypeScript에서는 `replace()` 메서드를 사용하여 텍스트를 검색하고 교체합니다. 이 메서드는 두 개의 인자를 받습니다: 찾을 문자열 혹은 정규 표현식, 그리고 교체할 문자열 혹은 함수입니다.

```TypeScript
let str = "Hello, World!";
let newStr = str.replace("World", "TypeScript");
console.log(newStr); // "Hello, TypeScript!"
```

정규 표현식을 이용하면, 모든 'World'를 'TypeScript'로 교체할 수 있습니다:

```TypeScript
let str = "Hello, World! World!";
let newStr = str.replace(/World/g, "TypeScript");
console.log(newStr); // "Hello, TypeScript! TypeScript!"
```

## 깊게 들어가봅시다

`replace()` 메서드는 ECMAScript 사양의 일부로, 단일 문자열이나 정규 표현식을 통해 순차적으로 검색합니다. 이는 원래 문자열을 변경시키진 않지만, 대신 새로운 문자열을 반환합니다.

대안으로 `String.prototype.replaceAll()` 메서드가 최신 ECMAScript 사양에서 도입되어 모든 인스턴스를 한 번에 교체할 수 있습니다. 하지만 아직 모든 TypeScript 환경에서 지원되지 않음을 유념하세요.

```TypeScript
let str = "Hello, World! World!";
let newStr = str.replaceAll("World", "TypeScript");
console.log(newStr); // "Hello, TypeScript! TypeScript!"
```

## 참고 레퍼런스

- MDN Web Docs에서는 `replace()`, `replaceAll()` 메서드에 대한 더 많은 정보를 제공합니다:
    - [replace()](https://developer.mozilla.org/ko/docs/Web/JavaScript/Reference/Global_Objects/String/replace)
    - [replaceAll()](https://developer.mozilla.org/ko/docs/Web/JavaScript/Reference/Global_Objects/String/replaceAll)

- TypeScript 공식 문서를 참조하면 우선 환경과 도구 세트에서 지원되는 최신 ECMAScript 기능을 확인할 수 있습니다:
    - [TypeScript 홈페이지](https://www.typescriptlang.org/)