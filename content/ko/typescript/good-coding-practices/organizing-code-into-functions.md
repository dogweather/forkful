---
aliases:
- /ko/typescript/organizing-code-into-functions/
date: 2024-01-26 01:16:28.959593-07:00
description: "\uCF54\uB4DC\uB97C \uD568\uC218\uB85C \uAD6C\uC131\uD55C\uB2E4\uB294\
  \ \uAC83\uC740 \uCF54\uB4DC\uB97C \uC7AC\uC0AC\uC6A9 \uAC00\uB2A5\uD558\uACE0 \uBAA8\
  \uB4C8\uD654\uB41C \uBE14\uB85D\uC73C\uB85C \uB098\uB204\uB294 \uAC83\uC744 \uC758\
  \uBBF8\uD569\uB2C8\uB2E4. \uC6B0\uB9AC\uB294 \uC774\uAC83\uC744 DRY(Don't Repeat\
  \ Yourself, \uBC18\uBCF5\uD558\uC9C0 \uB9C8\uB77C)\uB97C \uC720\uC9C0\uD558\uAE30\
  \ \uC704\uD574, \uCF54\uB4DC\uB97C \uB354 \uAE68\uB057\uD558\uACE0 \uC77D\uAE30\
  \ \uC27D\uAC8C, \uB514\uBC84\uADF8\uD558\uAE30 \uC27D\uAC8C \uB9CC\uB4E4\uAE30 \uC704\
  \uD574\uC11C \uD569\uB2C8\uB2E4."
lastmod: 2024-02-18 23:09:05.828000
model: gpt-4-0125-preview
summary: "\uCF54\uB4DC\uB97C \uD568\uC218\uB85C \uAD6C\uC131\uD55C\uB2E4\uB294 \uAC83\
  \uC740 \uCF54\uB4DC\uB97C \uC7AC\uC0AC\uC6A9 \uAC00\uB2A5\uD558\uACE0 \uBAA8\uB4C8\
  \uD654\uB41C \uBE14\uB85D\uC73C\uB85C \uB098\uB204\uB294 \uAC83\uC744 \uC758\uBBF8\
  \uD569\uB2C8\uB2E4. \uC6B0\uB9AC\uB294 \uC774\uAC83\uC744 DRY(Don't Repeat Yourself,\
  \ \uBC18\uBCF5\uD558\uC9C0 \uB9C8\uB77C)\uB97C \uC720\uC9C0\uD558\uAE30 \uC704\uD574\
  , \uCF54\uB4DC\uB97C \uB354 \uAE68\uB057\uD558\uACE0 \uC77D\uAE30 \uC27D\uAC8C,\
  \ \uB514\uBC84\uADF8\uD558\uAE30 \uC27D\uAC8C \uB9CC\uB4E4\uAE30 \uC704\uD574\uC11C\
  \ \uD569\uB2C8\uB2E4."
title: "\uCF54\uB4DC\uB97C \uD568\uC218\uB85C \uAD6C\uC131\uD558\uAE30"
---

{{< edit_this_page >}}

## 무엇과 왜?
코드를 함수로 구성한다는 것은 코드를 재사용 가능하고 모듈화된 블록으로 나누는 것을 의미합니다. 우리는 이것을 DRY(Don't Repeat Yourself, 반복하지 마라)를 유지하기 위해, 코드를 더 깨끗하고 읽기 쉽게, 디버그하기 쉽게 만들기 위해서 합니다.

## 방법:
기본 계산기를 만든다고 상상해 보세요. 필요할 때마다 덧셈 로직을 계속 작성하는 대신 `add` 함수를 만듭니다:

```TypeScript
function add(x: number, y: number): number {
  return x + y;
}

console.log(add(5, 7)); // 예제 출력: 12
```

이제, 우리가 곱하기 기능을 하는 함수가 필요하다고 가정해 봅시다:

```TypeScript
function multiply(x: number, y: number): number {
  return x * y;
}

console.log(multiply(3, 4)); // 예제 출력: 12
```
함수 당 하나의 작업에 집중하는 것을 어떻게 보셨나요? 그것이 바로 코드를 구성하는 핵심입니다.

## 깊이 있게
역사적으로, 프로그래밍 언어가 발전함에 따라, 함수는 코드 구조를 그리는 데 있어서 수학적 함수에서 가져온 핵심이 되었습니다. 그것들은 절차적 프로그래밍에서 필수적이며 객체 지향 및 함수형 프로그래밍 패러다임에서도 살아남았습니다.

대안들? 함수를 사용하지 않을 수도 있지만, 그것은 스파게티 타운으로 가는 일방통행 입니다. 또는 OOP(객체 지향 프로그래밍)을 선택하여 기능을 메소드에 패키징할 수 있습니다 - 이것은 기본적으로 객체에 속하는 함수입니다.

구현 측면에서, TypeScript는 타입에 주목합니다. 함수의 입력 및 출력 타입을 정의하는 것은 좋은 매너일 뿐만 아니라 깨끗한 TypeScript 코드를 위한 필수입니다. 게다가, TypeScript를 사용하면 오버로드, 제네릭, 선택적 매개변수와 같은 멋진 기능을 사용하여 함수를 강화할 수 있습니다.

## 또한 보기
함수 게임을 한 단계 끌어올리기 위한 이러한 자료들을 확인하세요:

- [TypeScript 핸드북 – 함수](https://www.typescriptlang.org/docs/handbook/2/functions.html): TypeScript 함수에 대한 당신의 성경입니다.
- [깨끗한 코드 자바스크립트](https://github.com/ryanmcdermott/clean-code-javascript#functions): 자바스크립트 함수에 깨끗한 코드 원칙을 적용하세요.
- [당신이 모르는 JS - 스코프 & 클로져](https://github.com/getify/You-Dont-Know-JS): 자바스크립트에서 함수가 스코프와 클로져와 어떻게 작동하는지 파악해보세요.
