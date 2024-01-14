---
title:                "TypeScript: 스트링의 길이 찾기"
simple_title:         "스트링의 길이 찾기"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/typescript/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## 왜
문자열의 길이를 찾는 것은 TypeScript 프로그래밍에서 매우 유용한 기술입니다. 이 기술을 사용하면 공백을 포함한 모든 문자를 세고, 다양한 문자열 함수와 조합하여 다양한 작업을 수행할 수 있습니다.

## 사용 방법
우선, 문자열을 변수에 저장해야 합니다. 그리고 이 변수에 `.length`를 붙여주면 해당 문자열의 길이를 구할 수 있습니다. 예를 들어, 다음과 같습니다.

```TypeScript
let str: string = "안녕하세요!";
console.log(str.length);

// 출력 결과
// 6
```

위 예시에서 `str` 변수에는 한글이 포함된 문자열이 저장되어 있습니다. 그리고 `console.log()`를 이용해 `str.length`를 출력하면 해당 문자열의 길이인 6이 출력됩니다. 마찬가지로, 영문이 포함된 문자열도 길이를 구할 수 있습니다.

```TypeScript
let str: string = "Hello, world!";
console.log(str.length);

// 출력 결과
// 13
```

## 깊게 들어가기
문자열의 길이를 구하는 메소드는 내부적으로 `for` 루프를 사용하여 공백을 포함한 모든 문자의 개수를 세줍니다. 그리고 그 값을 리턴하게 됩니다. 따라서, 특정 문자열의 길이가 아주 길 경우에는 `for` 루프를 사용하므로 성능에 영향을 미칠 수 있으니 주의해야 합니다. 또한, 이 메소드를 사용할 때 공백을 처리하는 방식에 따라 결과값이 달라질 수 있습니다.

## 관련 링크
### [MDN - String.prototype.length](https://developer.mozilla.org/ko/docs/Web/JavaScript/Reference/Global_Objects/String/length)
MDN에서는 JavaScript의 `String.prototype.length`에 대해 더 자세한 설명과 예시를 제공합니다.

### [Tutorials Point - TypeScript Strings](https://www.tutorialspoint.com/typescript/typescript_strings.htm)
이 문서에서는 TypeScript에서 문자열을 다루는 방법과 `.length` 메소드에 대해 설명합니다.

### [Typescriptlang.org - String](https://www.typescriptlang.org/docs/handbook/2/typescript-in-5-minutes.html#string)
타입스크립트 공식 문서에서도 문자열과 관련된 내용을 다루고 있습니다. `.length` 메소드 외에도 다양한 문자열 메소드들을 확인할 수 있습니다.