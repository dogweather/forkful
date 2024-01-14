---
title:                "TypeScript: 명령 줄 인자 읽기"
simple_title:         "명령 줄 인자 읽기"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/typescript/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

##왜

프로그래밍에서 커맨드 라인 인자를 읽는 것이 왜 중요한지 궁금하셨나요? 커맨드 라인 인자를 제대로 이해하고 활용하는 것은 코드를 더욱 유연하고 강력하게 만들 수 있습니다.

##어떻게 하면 될까요?

커맨드 라인 인자를 읽는 것은 TypeScript에서도 간단하게 할 수 있습니다. 먼저, `process` 모듈을 이용하여 커맨드 라인 인자를 가져오는 방법을 살펴보겠습니다.

```TypeScript
// 코드 블록 1: 커맨드 라인 인자를 가져오는 예제

// process 모듈 불러오기
import process from 'process';

// 인자 배열 가져오기
const args = process.argv;

// 첫 번째 인자 출력
console.log(`첫 번째 인자: ${args[0]}`);
```

위의 코드 블록을 실행하면 커맨드 라인에서 실행한 파일의 경로가 첫 번째 인자로 출력될 것입니다. 또 다른 예제를 살펴보겠습니다.

```TypeScript
// 코드 블록 2: 숫자 인자의 합 구하기

// process 모듈 불러오기
import process from 'process';

// 인자 배열 가져오기
const args = process.argv;

// 첫 번째 인자는 파일 경로이므로 제외하고 두 번째 인자부터 합을 구합니다
let sum = 0;
for (let i = 2; i < args.length; i++) {
  sum += parseInt(args[i]);
}

// 합 출력
console.log(`숫자 인자의 합: ${sum}`);
```

위의 코드를 실행하면 커맨드 라인에서 공백으로 구분된 숫자를 입력하면 그 숫자들의 합이 출력될 것입니다. 이처럼 커맨드 라인 인자를 활용하여 프로그램의 인자를 직접 전달할 수도 있습니다.

##심화 학습

커맨드 라인 인자를 읽는 방법에 대해 더 깊이 알아보겠습니다. `process.argv` 배열에는 첫 번째 인자로 파일 경로가 들어오고, 두 번째 인자부터 사용자의 입력이 들어옵니다. 이 인자들은 모두 문자열 형태로 저장되니 필요에 따라서 적절한 형변환을 해주어야 합니다. 또한, `process.argv` 이외에도 `process.argv0`을 통해 실행한 Node.js의 경로를 참조할 수 있습니다.

##참고 자료

- [TypeScript Handbook: Command Line Arguments](https://www.typescriptlang.org/docs/handbook/release-notes/typescript-4-2.html#support-for-command-line-arguments)
- [Node.js v14.16.0 Documentation: Process](https://nodejs.org/api/process.html#process_process_argv)
- [Understanding Command Line Arguments in Node.js](https://www.digitalocean.com/community/tutorials/how-to-read-command-line-arguments-in-node-js)

##더 알아보기