---
title:                "디버그 출력하기"
html_title:           "TypeScript: 디버그 출력하기"
simple_title:         "디버그 출력하기"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/typescript/printing-debug-output.md"
---

{{< edit_this_page >}}

# 무엇 & 왜?
디버그 출력을 프로그래머들이 하는 이유는 오류를 찾고 수정하기 위해서입니다. 이는 코드 내부의 특정 부분이 어떻게 작동하는지 확인하고 문제를 해결하는 데 도움이 됩니다.

# 어떻게:
**타입스크립트** 코드 블록 내부에서 예제와 출력을 살펴보겠습니다.
```TypeScript
let num1 = 5;
let num2 = 10;
console.log(`덧셈 결과: ${num1 + num2}`);
```
**출력**: `덧셈 결과: 15`

```TypeScript
let str1 = "Hello";
let str2 = "World";
console.log(`${str1} ${str2}!`);
```
**출력**: `Hello World!`


# 깊이 파고들기:
디버그 출력은 오래 전부터 프로그래머들이 사용한 도구입니다. 다양한 언어에서 출력 함수의 이름이나 구문은 다르지만, 기본적인 목적은 비슷합니다. 프로그래머들은 출력을 사용하여 코드의 특정 부분이 어떻게 실행되는지 살펴볼 수 있고, 오류를 찾고 수정할 수 있습니다. 디버깅 과정에서 출력을 사용하는 대신 디버거를 사용할 수도 있지만, 일부 상황에서는 디버그 출력이 더 효율적일 수 있습니다.

# 더 알아보기:
- [타입스크립트 공식 문서: 디버깅](https://www.typescriptlang.org/docs/handbook/debugging.html)
- [디버깅을 위한 타입스크립트 로깅: console.log, console.debug, console.error 차이점 알아보기](https://levelup.gitconnected.com/whats-the-difference-between-console-log-debug-and-error-in-javascript-39275b1ec933)
- [디버그 출력을 지원하는 다른 프로그래밍 언어들 알아보기](https://en.wikipedia.org/wiki/Debug_output)