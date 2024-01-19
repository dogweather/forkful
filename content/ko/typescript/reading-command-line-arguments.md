---
title:                "명령줄 인수 읽기"
html_title:           "Arduino: 명령줄 인수 읽기"
simple_title:         "명령줄 인수 읽기"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/typescript/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## 무엇 & 왜?

명령 줄 인자를 읽는 것은 프로그램에 전달된 입력을 파악하는 방법입니다. 이것은 프로그램이 실행되는 방식의 유연성과 사용자 반응성을 높이기 위해 프로그래머가 활용하는 흔한 기법입니다.

## 어떻게 하는가:

명령 줄 인자를 TypeScript로 다루는 방법을 살펴보겠습니다. 값 접근은 프로세스 객체의 `argv` 배열을 통해 이루어집니다.

```TypeScript
// TypeScript 예시 코드
let myArgs = process.argv.slice(2);

console.log('myArgs:', myArgs);
``` 

이 코드를 `node app.js arg1 arg2` 와 같이 실행하면, 출력 결과는 아래와 같습니다.

```
myArgs: [ 'arg1', 'arg2' ]
```

## 깊이 파기:

프로그램에 명령 줄 인자를 전달하는 개념은 오래된 것이며, Unix와 같은 일부 최초의 운영 시스템부터 사용되었습니다. 좀 더 복잡한 상황에서는 option parsing libraries(e.g., commander, yargs)를 사용하여 도움을 얻을 수 있습니다. 또한, 이러한 라이브러리들은 디폴트 값, 타입 체크를 가능하게 해주어 명령줄 인자를 파싱하는 복잡성을 줄여줍니다.

## 참고:

- [Node.js documentation: process.argv](https://nodejs.org/docs/latest/api/process.html#process_process_argv)
- [Commander.js Package for command-line interfaces](https://www.npmjs.com/package/commander)
- [Yargs.js Parsing Library](https://yargs.js.org/)