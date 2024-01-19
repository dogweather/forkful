---
title:                "명령줄 인수 읽기"
html_title:           "Arduino: 명령줄 인수 읽기"
simple_title:         "명령줄 인수 읽기"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/javascript/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

# 명령행 인수 읽기

## 무엇이며 왜 사용하나요?
명령행 인수는 사용자가 터미널을 통해 프로그램에 전달하는 인자들입니다. 이를 통해 개발자들은 다양한 동작 및 설정을 유동적으로 조정하며 프로그램을 실행할 수 있습니다.

## 다음과 같이 수행하십시오 :
Javascript 에서는 `process.argv`를 사용하여 명령행 인수를 읽을 수 있습니다. `process.argv`는 배열 형태로 제공되며, 첫 번째 요소는 'node', 두 번째 요소는 실행 파일의 경로를 가리킵니다.

```Javascript
console.log(process.argv);
```
위 예제에서 명령행 인수를 출력하면 다음과 같이 표시됩니다.

```Javascript
[ '/usr/local/bin/node', '/path/to/your/script.js', 'arg1', 'arg2' ]
```

특정 인수만 읽을 경우 다음과 같이 수행할 수 있습니다.

```Javascript
const myArg = process.argv[2];
console.log(myArg);
```
## 깊이 파보기
명령행 인수는 초기 Unix와 같은 운영체제에서 시작되었습니다. 이런 패러다임은 사용자가 터미널 또는 쉘에서 어떤 동작을 제어할 수 있게 해줍니다. `process.argv` 외에도 `yargs`나 `commander`와 같은 라이브러리를 통해 보다 구조화된 방식으로 인수를 처리할 수 있습니다. 

## 같이 보기
* Node.js 공식 문서: [process.argv](https://nodejs.org/docs/latest/api/process.html#process_process_argv)
* Yargs 와 함께 사용하기: [Yargs Github](https://github.com/yargs/yargs)
* Commander 와 함께 사용하기: [Commander.js Github](https://github.com/tj/commander.js/)