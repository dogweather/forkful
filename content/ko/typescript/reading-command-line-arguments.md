---
title:    "TypeScript: 컴퓨터 프로그래밍에서 명령줄 인수 읽기"
keywords: ["TypeScript"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/ko/typescript/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## 왜

커맨드 라인 인자를 읽는 것이 왜 중요한지 궁금하신가요? 이 글을 읽으면서 이유를 알아보세요.

## 어떻게

커맨드 라인 인자를 읽는 방법을 알려드리겠습니다. 먼저, ```process.argv```를 사용하여 인자를 읽을 수 있습니다. 아래의 예시 코드를 참고해보세요.

```TypeScript
const args: string[] = process.argv;
console.log("첫 번째 인자는: " + args[2]);
console.log("두 번째 인자는: " + args[3]);
```

위 코드를 실행하면, 콘솔에 첫 번째 인자와 두 번째 인자의 값을 출력할 수 있습니다.

## 심층 분석

커맨드 라인 인자를 읽는 것은 프로그래밍에서 상당히 중요한 기술입니다. 인자를 읽어서 사용자에게 적절한 응답을 보내는 것이 가능해집니다. 또한, 사용자로부터 입력받은 데이터를 변수에 저장하여 나중에 사용할 수 있습니다.

## 참고 자료

- [Node.js 공식 문서 - process.argv](https://nodejs.org/api/process.html#process_process_argv)
- [TypeScript 핸드북 - 커맨드 라인 인자](https://www.typescriptlang.org/docs/handbook/utility-types.html#type-parameters-and-tuple-types)
- [Node.js에서 커맨드 라인 인자 읽기](https://stackoverflow.com/questions/4351521/how-do-i-pass-command-line-arguments-to-a-node-js-program)

## 참고하세요

이 글을 읽고 커맨드 라인 인자를 읽는 방법을 배우셨다면, 이제 커맨드 라인 인자를 활용하여 더 다양한 프로그래밍 문제를 해결해보세요!