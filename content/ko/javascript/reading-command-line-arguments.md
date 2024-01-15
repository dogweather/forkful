---
title:                "컴퓨터 프로그래밍에서 명령 줄 인수 읽기"
html_title:           "Javascript: 컴퓨터 프로그래밍에서 명령 줄 인수 읽기"
simple_title:         "컴퓨터 프로그래밍에서 명령 줄 인수 읽기"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/javascript/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## 왜?

만약 당신이 프로그래밍을 배우고 있다면, 커맨드 라인 인자를 읽는 것은 당신의 기술을 향상시키는 데 도움이 됩니다.

이것은 당신이 프로그램을 실행할 때 입력하는 인자들을 읽고 그에 맞게 프로그램을 동작시킬 수 있도록 합니다.

## 어떻게 할까요?

커맨드 라인 인자를 읽는 것은 간단한 작업입니다. 우선, `process.argv`라는 내장 변수를 사용하여 프로그램이 받은 모든 인자를 배열로 저장합니다. 그리고 `for loop`를 사용하여 이 배열을 탐색하면서 각각의 인자를 읽고 원하는 작업을 수행할 수 있습니다.

예제 코드를 살펴보겠습니다.

```Javascript
// process.argv를 사용하여 모든 커맨드 라인 인자를 읽어옵니다.
let args = process.argv;

// 첫 번째 인자는 항상 실행되는 프로그램의 경로이므로 무시합니다.
// 두 번째 인자부터 원하는 작업을 수행합니다.
for (let i = 2; i < args.length; i++) {
  console.log("인자 " + i + " : " + args[i]);
}

// 예제로 실행해보면 다음과 같은 결과가 출력됩니다.
// node index.js hello world
// 인자 2 : hello
// 인자 3 : world
```

이렇게 간단하게 커맨드 라인 인자를 읽고 활용할 수 있습니다.

## 더 깊이 파보기

커맨드 라인 인자를 읽는 것 외에도, Node.js에서 제공하는 다양한 내장 모듈들을 사용하면 더욱 다양한 작업을 수행할 수 있습니다.

예를 들어, `yargs`라는 외부 라이브러리를 사용하면 더 쉽게 인자를 읽고 활용할 수 있습니다.

자세한 내용은 공식 문서를 참고해보세요.

## See Also

- [Node.js 공식 문서](https://nodejs.org/api/process.html#process_process_argv)
- [yargs 공식 문서](https://github.com/yargs/yargs)