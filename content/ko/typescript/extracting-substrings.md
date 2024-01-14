---
title:                "TypeScript: 문자열 추출하기"
simple_title:         "문자열 추출하기"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/typescript/extracting-substrings.md"
---

{{< edit_this_page >}}

## 왜

자바스크립트에서 substr 메소드를 사용하여 문자열에서 하위 문자열을 추출하는 방법에 대해 알아보겠습니다.

## 방법

우선, substr 메소드를 사용하기 전에 해당 문자열을 변수에 할당해야 합니다. 그리고 원하는 하위 문자열을 추출할 시작 인덱스와 끝 인덱스를 지정해야 합니다. 시작 인덱스는 해당 문자열에서 추출하고 싶은 문자열의 첫 번째 문자의 위치를 의미합니다. 끝 인덱스는 추출하고 싶은 문자열보다 1 더 큰 값의 위치를 의미합니다.

```TypeScript
const str = "안녕하세요, TypeScript 블로그입니다.";
const subStr = str.substr(6, 9); 
console.log(subStr); // 출력 결과: TypeScript
```

위 예시에서는 해당 문자열에서 6번 인덱스인 "T"부터 9번 인덱스인 "t"까지인 "TypeScript"를 추출하게 됩니다.

## 딥 다이브

substr 메소드를 사용할 때 주의해야 할 점이 있습니다. 먼저, 시작 인덱스와 끝 인덱스는 항상 양의 정수 값을 사용해야 합니다. 음수를 사용하면 예상치 못한 결과가 나올 수 있습니다. 또한, 끝 인덱스를 생략하면 시작 인덱스부터 해당 문자열의 끝까지 추출하게 됩니다.

또한, substr 메소드는 문자열에서 하위 문자열을 추출하는 것 이외에도 문자열의 길이를 조정하는 데에도 사용될 수 있습니다. 예를 들어, 시작 인덱스를 0으로 지정하고 끝 인덱스를 음수 값으로 지정하면 해당 문자열의 끝에서부터 따라오는 문자열들을 제거하게 됩니다. 또는 음수 값을 시작 인덱스로 지정하면 해당 문자열의 끝에서부터 역순으로 지정된 문자열의 길이만큼 제거하게 됩니다.

## 참고자료

[MDN Web Docs - substr 메소드](https://developer.mozilla.org/ko/docs/Web/JavaScript/Reference/Global_Objects/String/substr)  
[TypeScript 공식 홈페이지](https://www.typescriptlang.org/)  
[JavaScript에서 문자열 다루기](https://velog.io/@chloeee/TypeScript-이한글변환-폰트-저작물-세종시트YPE-설치)