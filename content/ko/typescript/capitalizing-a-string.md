---
title:                "문자열 대문자로 바꾸기"
html_title:           "TypeScript: 문자열 대문자로 바꾸기"
simple_title:         "문자열 대문자로 바꾸기"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/typescript/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## 무엇이며 왜 사용하나요?

문자열 대문자화는 문자열의 모든 문자를 대문자로 변환하는 작업을 의미합니다. 이는 사용자 입력의 일관성을 유지하거나, 특정 텍스트를 강조하는 등 다양한 이유로 프로그래머들이 사용합니다.

## 어떻게 사용하나요:

다음의 TypeScript 코드 예시를 참조하세요:
```TypeScript
let str = "hello world";
let upperStr = str.toUpperCase();
console.log(upperStr); // 출력: "HELLO WORLD"
```

## 깊이 들여다보기:

*사실 이야기*: 원래 알파벳을 대문자화하는 것은 타자기를 이용하는 초창기 언론사에서 시작되었습니다. 그 때는 헤드라인을 강조하기 위해 대문자를 주로 사용하였습니다.

*대체 방법*: TypeScript 외에도, JavaScript 에서도 `toUpperCase()`와 동일한 기능을 가진 메소드를 사용할 수 있습니다. 아래는 그 예시입니다:
```JavaScript
let str = "hello world";
let upperStr = str.toUpperCase();
console.log(upperStr); // 출력: "HELLO WORLD"
```

*구현 세부 정보*: `toUpperCase()` 메소드는 문자열에 대해 작동할 뿐만 아니라, 숫자형 과 null 값에 대해서도 작동합니다. 그러나 객체에 대해서 작동하지는 않습니다. 예시는 아래와 같습니다:
```TypeScript
let num = 123;
let numToString = num.toString();
let upperNumToString = numToString.toUpperCase();
console.log(upperNumToString); // 출력: "123"
```

## 참고하기 위한 링크:

- [toUpperCase() 메소드 MDN 문서](https://developer.mozilla.org/ko/docs/Web/JavaScript/Reference/Global_Objects/String/toUpperCase)
- [타입스크립트 공식 문서](https://www.typescriptlang.org/docs/)