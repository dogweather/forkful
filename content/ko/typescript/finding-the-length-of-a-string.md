---
title:                "문자열의 길이 찾기"
html_title:           "TypeScript: 문자열의 길이 찾기"
simple_title:         "문자열의 길이 찾기"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/typescript/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## 왜

문자열의 길이를 찾는 것이 왜 중요한지 궁금해 한다면, TypeScript로 이를 구현하는 방법을 익히는 것은 매우 유용한 기술입니다. 실제로 문자열의 길이를 찾는 것은 프로그래밍에서 매우 일반적인 수행 작업이기 때문입니다.

## 어떻게

```
TypeScript
let str: string = "안녕하세요";
console.log(str.length); // Output: 5 
```

문자열을 변수에 할당하고 .length 속성을 사용하여 문자열의 길이를 찾을 수 있습니다. 이렇게하면 출력값으로 문자열의 실제 길이가 표시됩니다. 이를 통해 우리는 문자열을 구성하는 문자의 개수를 알 수 있습니다.

이것은 TypeScript에서 기본적으로 제공하는 기능이지만, 우리가 유용하게 활용할 수 있도록 다른 방식으로도 문자열의 길이를 찾는 방법이 있습니다. 예를 들어, for 반복문을 사용하여 문자열의 각 문자를 순회하는 방법도 있습니다. 이를 통해 우리는 문자열의 길이를 측정할 수 있습니다.

## 깊이 파고들기

우리는 일반 문자열 뿐만 아니라 여러 언어와 문자 집합을 다루는 경우에도 TypeScript에서 문자열의 길이를 쉽게 찾을 수 있습니다. 이는 TypeScript가 문자열을 유니코드로 다루기 때문입니다. 따라서 우리는 TypeScript를 사용하여 한글, 영어, 중국어 등 다양한 언어로 이루어진 문자열의 길이를 정확하게 확인할 수 있습니다.

See Also

- [TypeScript 문자열 관련 문서](https://www.typescriptlang.org/docs/handbook/basic-types.html#string)
- [유니코드 문자열 길이 계산](https://stackoverflow.com/questions/55172034/typescript-length-of-unicode-string)
- [알파벳, 숫자, 특수문자로 이루어진 문자열 길이 계산 예제](https://dev.to/selbekk/how-to-get-the-length-of-a-string-in-typescript-409j)