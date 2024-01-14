---
title:    "TypeScript: 문자열 연결하기"
keywords: ["TypeScript"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/ko/typescript/concatenating-strings.md"
---

{{< edit_this_page >}}

## 왜
문자열을 연결하는 것에 대해 공부하는 것은 TypeScript 프로그래밍에서 필수적입니다. 문자열을 결합하면 사용자와 상호작용하는 데 유용한 정보를 제공할 수 있으며 코드를 간결하고 효율적으로 유지할 수 있습니다.

## 하는 법
```TypeScript
// 두 개의 문자열을 결합하는 예제
let text1: string = "안녕하세요";
let text2: string = "저는 TypeScript를 공부 중입니다.";
let combinedText: string = text1 + text2;

console.log(combinedText); // 출력결과: 안녕하세요저는 TypeScript를 공부 중입니다.
```

```TypeScript
// 문자열을 수량에 따라 반복해 결합하는 예제
let text: string = "안녕하세요";
let repeatedText: string = text.repeat(3);

console.log(repeatedText); // 출력결과: 안녕하세요안녕하세요안녕하세요
```

```TypeScript
// 문자열과 변수를 결합하는 예제
let name: string = "예린";
let age: number = 23;
let message: string = `안녕하세요, 제 이름은 ${name}이고 제 나이는 ${age}살입니다.`;

console.log(message); // 출력결과: 안녕하세요, 제 이름은 예린이고 제 나이는 23살입니다.
```

## 딥 다이브
문자열을 연결하는 데는 다양한 방법이 있습니다. 위의 예제 외에도 `concat()` 함수나 템플릿 리터럴을 사용하는 방법 등이 있습니다. 또한 문자열을 결합할 때 유의해야할 점은 문자열과 숫자를 함께 결합할 때 발생하는 자료형 변환 오류입니다. 이를 해결하기 위해 `toString()` 함수나 `String()` 함수를 사용할 수 있습니다.

## 참고 자료
- [TypeScript 문자열 결합 방법](https://www.typescriptlang.org/docs/handbook/variable-declarations.html#string-concatenation)
- [TypeScript 템플릿 리터럴 사용법](https://www.typescriptlang.org/docs/handbook/variable-declarations.html#template-literals)
- [자료형 변환 오류 해결 방법](https://medium.com/better-programming/handling-string-and-number-conversion-errors-2331e8c04b74)

## 더 알아보기