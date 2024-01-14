---
title:                "TypeScript: 패턴과 일치하는 문자 삭제"
programming_language: "TypeScript"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/typescript/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## 왜

여러분은 TypeScript 프로그래머일지라도 때때로 문자열에서 특정한 패턴과 일치하는 문자를 삭제해야 할 수 있습니다. 이런 작업은 문제 해결에 필수적일 수 있으며 효율적인 프로그래밍을 위해 꼭 알아두어야 할 기능입니다.

## 방법

```TypeScript
const targetString: string = "Hello World!";
const pattern: RegExp = /l/g;

const result: string = targetString.replace(pattern, "");

console.log(result); // "Heo Word!"
```

위의 예제 코드는 문자열에서 `l` 이라는 문자를 전부 삭제하는 간단한 예시입니다. `replace` 함수에 대한 사용법을 이해하고 있다면 다른 패턴을 사용해 원하는 결과를 얻을 수 있을 것입니다.

## 딥 다이브

TypeScript에서 `replace` 함수는 문자열에서 원하는 패턴과 일치하는 부분을 찾아 다른 문자로 변경하는 역할을 합니다. 이 함수에는 정규표현식을 사용할 수 있습니다. 정규표현식은 패턴을 정의하는 데 사용되며 매우 강력한 기능을 제공합니다. 예를 들어, `l` 이외의 문자, 즉 모음만 삭제하려면 `/[^aeiou]/g`와 같이 정규표현식을 사용할 수 있습니다.

또한 `replace` 함수는 콜백 함수를 전달할 수도 있습니다. 이를 통해 문자열의 각 부분에 대해 복잡한 작업을 수행할 수 있습니다. 자세한 내용은 TypeScript 공식 문서를 참고하시기 바랍니다.

## 참고하십시오

- [TypeScript 공식 문서 - 문자열](https://www.typescriptlang.org/docs/handbook/basic-types.html#string)
- [MDN 웹 문서 - 정규표현식](https://developer.mozilla.org/ko/docs/Web/JavaScript/Guide/Regular_Expressions)
- [MDN 웹 문서 - String.prototype.replace()](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/String/replace)