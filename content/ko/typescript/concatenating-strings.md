---
title:                "TypeScript: 문자열 연결하기"
simple_title:         "문자열 연결하기"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/typescript/concatenating-strings.md"
---

{{< edit_this_page >}}

## 왜

이번 포스트에서는 TypeScript에서 문자열을 연결하는 방법에 대해 알아보겠습니다. 문자열 연결은 프로그래밍에서 매우 일반적으로 사용되는 작업이며, 새로운 문자열을 만들거나 필요한 정보를 표시하기 위해 사용됩니다.

## 이 방법으로

우선, 우리는 두 개의 문자열을 함께 붙인다는 것을 알고 있어야 합니다. 다음 코드 블록을 보시죠.

```TypeScript
let firstString = '안녕하세요,';
let secondString = '저는 TypeScript 입니다.';
let result = firstString + secondString;
console.log(result);
```

위 코드를 실행해보면, "안녕하세요, 저는 TypeScript 입니다." 라는 출력 결과를 볼 수 있습니다. 우리는 `+` 연산자를 사용하여 두 문자열을 연결하였는데요, 이렇게 쉽게 문자열을 연결할 수 있습니다.

또한, 우리는 템플릿 리터럴을 사용하여도 문자열을 연결할 수 있습니다. 코드 블록을 보시죠.

```TypeScript
let firstString = '안녕하세요,';
let secondString = '저는 TypeScript 입니다.';
let result = `${firstString} ${secondString}`;
console.log(result);
```

위 코드를 실행하면 같은 출력 결과를 볼 수 있습니다. 템플릿 리터럴 내에서 `${ }`를 사용하면 변수값을 문자열에 삽입할 수 있습니다.

## 깊게 들어가기

TypeScript에서 문자열을 연결하는 데는 두 가지 방법이 있습니다. 첫 번째는 `+` 연산자를 사용하는 것이고, 두 번째는 템플릿 리터럴을 사용하는 것입니다. 또한, 우리는 `.concat()` 메서드를 사용하여 여러 문자열을 연결할 수도 있습니다. 예제 코드를 보시죠.

```TypeScript
let firstString = '안녕하세요,';
let secondString = '저는 TypeScript 입니다.';
let thirdString = '문자열을 연결하는';
let result = firstString.concat(secondString, thirdString);
console.log(result);
```

위 코드를 실행하면 "안녕하세요, 저는 TypeScript 입니다. 문자열을 연결하는" 라는 출력 결과를 볼 수 있습니다. `.concat()` 메서드에 여러 개의 매개변수를 전달하면 모든 문자열이 연결되어 출력됩니다.

## 또 다른 예제 코드

아래는 더 많은 예제 코드입니다. 각 예제는 다른 방식으로 문자열을 연결하는 방법을 보여줍니다.

- [TypeScript에서 문자열 연결하기 예제 코드](https://www.typescriptlang.org/docs/handbook/basic-types.html#string-concatenation)

## 더 알아보기

우리는 이번 포스트에서 TypeScript에서 문자열을 연결하는 방법을 알아보았습니다. 문자열 연결은 프로그래밍에서 매우 중요한 작업이므로, 효과적으로 활용할 수 있도록 더 많은 예제 코드와 문서를 참고하시기 바랍니다.

## 관련 링크

- [TypeScript 문서](https://www.typescriptlang.org/)
- [TypeScript 튜토리얼](https://www.typescriptlang.org/docs/handbook/typescript-in-5-minutes.html)
- [TypeScript 공식 블로그](https://blog.typescriptlang.org/)