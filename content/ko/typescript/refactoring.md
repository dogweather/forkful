---
title:                "리팩토링"
aliases:
- ko/typescript/refactoring.md
date:                  2024-01-26T03:37:04.707163-07:00
model:                 gpt-4-0125-preview
simple_title:         "리팩토링"

tag:                  "Good Coding Practices"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/typescript/refactoring.md"
---

{{< edit_this_page >}}

## 무엇 & 왜?
리팩터링은 기존 컴퓨터 코드의 구조를 바꾸는 과정이며, 외부 동작을 변경하지 않습니다. 프로그래머는 코드를 더 깨끗하게 하고, 유지보수하기 쉽게 만들며, 복잡성을 줄여서 새로 파고드는 이에게 이해하기 쉽게 하기 위해 이 작업을 합니다.

## 어떻게:
잘 보지 않은 날이 더 많은 TypeScript 함수가 있는데, 이 함수는 조금 지저분하고 약간의 애정과 관심이 필요합니다:

```typescript
function userInfo(data: any): string {
    return "User Info: " + data.name + ", " + data.age + ", " + data.email + ";" ;
}
```
리팩터링을 거친 이 코드는 다음과 같을 수 있습니다:

```typescript
interface User {
    name: string;
    age: number;
    email: string;
}

function formatUserInfo(user: User): string {
    return `User Info: ${user.name}, ${user.age}, ${user.email};`;
}
```

두 번째 예제는 TypeScript의 타입 시스템을 `interface`로 활용하여 런타임 오류의 가능성을 피하고 가독성을 향상시킨, 더 견고한 방식입니다.

## 자세히
리팩터링은 현대적인 개념이 아니며, 프로그래밍과 함께 발전해왔으며, 마틴 파울러의 "Refactoring: Improving the Design of Existing Code" 책이 1999년에 출판되면서 더욱 체계화되었습니다. 이는 애자일 개발 환경에서 중요한 역할을 하여 코드 변경을 유연하게 합니다. 수동 리팩터링에 대한 대안으로는 TSLint나 TypeScript 자체 언어 서버와 같은 자동화 도구가 있어 특정 리팩터링 작업을 제안하거나 심지어 수행할 수 있습니다. 구현 세부사항은 보통 중복 코드, 긴 메서드, 큰 클래스 등의 "코드 냄새"를 인식하고, 메서드 추출, 더 적합한 클래스로 이동, 더 단순한 구조 사용과 같은 패턴을 적용하여 해결하는 것을 포함합니다. 이러한 패턴은 리팩터링의 방법과 이유를 이해하는 데 핵심적입니다.

## 참고자료
- [마틴 파울러의 "Refactoring: Improving the Design of Existing Code" 책](https://martinfowler.com/books/refactoring.html)
- [정적 코드 분석을 위한 TSLint](https://palantir.github.io/tslint/)
- [코드 냄새 이해하기](https://refactoring.guru/refactoring/smells)
