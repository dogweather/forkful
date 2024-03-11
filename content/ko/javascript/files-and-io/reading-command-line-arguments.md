---
date: 2024-01-20 17:56:16.389163-07:00
description: "Node.js\uC5D0\uC11C \uBA85\uB839\uC904 \uC778\uC790\uB97C \uC77D\uB294\
  \ \uAC74 \uD504\uB85C\uADF8\uB7A8\uC5D0 \uC0AC\uC6A9\uC790 \uC785\uB825\uC744 \uC804\
  \uB2EC\uD558\uB294 \uBC29\uBC95\uC785\uB2C8\uB2E4. \uC124\uC815, \uD30C\uC77C \uACBD\
  \uB85C \uBC0F \uC791\uC5C5 \uC635\uC158 \uB4F1\uC744 \uB3D9\uC801\uC73C\uB85C \uC9C0\
  \uC815\uD560 \uB54C \uC0AC\uC6A9\uD569\uB2C8\uB2E4."
isCJKLanguage: true
lastmod: '2024-03-11T00:14:29.739226-06:00'
model: gpt-4-1106-preview
summary: "Node.js\uC5D0\uC11C \uBA85\uB839\uC904 \uC778\uC790\uB97C \uC77D\uB294 \uAC74\
  \ \uD504\uB85C\uADF8\uB7A8\uC5D0 \uC0AC\uC6A9\uC790 \uC785\uB825\uC744 \uC804\uB2EC\
  \uD558\uB294 \uBC29\uBC95\uC785\uB2C8\uB2E4. \uC124\uC815, \uD30C\uC77C \uACBD\uB85C\
  \ \uBC0F \uC791\uC5C5 \uC635\uC158 \uB4F1\uC744 \uB3D9\uC801\uC73C\uB85C \uC9C0\uC815\
  \uD560 \uB54C \uC0AC\uC6A9\uD569\uB2C8\uB2E4."
title: "\uBA85\uB839\uC904 \uC778\uC218 \uC77D\uAE30"
---

{{< edit_this_page >}}

## What & Why? (무엇인가요? 왜 사용하나요?)
Node.js에서 명령줄 인자를 읽는 건 프로그램에 사용자 입력을 전달하는 방법입니다. 설정, 파일 경로 및 작업 옵션 등을 동적으로 지정할 때 사용합니다.

## How to (어떻게 사용하나요?):
Node.js에서 명령줄 인자를 처리하는 기본 예제입니다.

```javascript
// process_argv.js 파일안에
// process.argv 배열로 명령줄 인자에 접근
process.argv.forEach((val, index) => {
  console.log(`${index}: ${val}`);
});

// 터미널에서 실행
// $ node process_argv.js one two=three four

// 콘솔 출력 예시
0: /path/to/your/node/executable
1: /path/to/your/script/process_argv.js
2: one
3: two=three
4: four
```
주의: 첫 두 인자는 각각 Node.js 실행파일 경로와 스크립트 파일 경로입니다.

## Deep Dive (깊이 알아보기):
명령줄 인자는 초기 UNIX 시스템에서부터 사용되어 왔습니다. 이를 통해 소프트웨어 유틸리티들이 유연성을 가지며 작업할 수 있도록 합니다.

Node.js가 등장하면서 `process.argv`을 통한 인자 읽기가 가능해졌지만, 더 나은 대안들도 있습니다:
- `yargs`: 복잡한 인자 파싱을 위해 인기 있는 라이브러리
- `commander`: 명령줄 인터페이스를 구축하기 위한 모듈
- `minimist`: 인자를 쉽게 분해하고 관리하는 데 도움을 주는 경량 모듈

`yargs`나 `commander`와 같은 도구들은 명령줄 인자를 통해 보다 복잡한 실행 로직을 구현할 때 유용합니다. 인자 파싱, 기본값 설정, 도움말 생성 등을 제공합니다.

## See Also (추가 정보):
- Node.js 공식 문서의 프로세스 문서: [https://nodejs.org/docs/latest/api/process.html#process_process_argv](https://nodejs.org/docs/latest/api/process.html#process_process_argv)
- `yargs` GitHub 페이지: [https://github.com/yargs/yargs](https://github.com/yargs/yargs)
- `commander` GitHub 페이지: [https://github.com/tj/commander.js](https://github.com/tj/commander.js)
