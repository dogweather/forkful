---
title:                "명령 줄 인수 읽기"
html_title:           "Javascript: 명령 줄 인수 읽기"
simple_title:         "명령 줄 인수 읽기"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/javascript/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## 무엇 & 왜?

커맨드 라인 아규먼트를 읽는 것은 프로그래머가 사용자로부터 입력을 받아와서 프로그램을 실행하는 데 필요한 정보를 얻는 과정입니다. 예를 들어, 사용자가 웹 브라우저를 실행할 때 웹 페이지의 주소를 커맨드 라인 아규먼트로 전달하면 해당 페이지를 띄우는 것과 같은 이유로, 프로그래머들은 커맨드 라인 아규먼트를 읽어옵니다.

## 사용 방법:

```Javascript
// process 객체를 사용하여 커맨드 라인 아규먼트 읽기
const args = process.argv;
console.log(args);

// 예시 입력과 출력

$ node index.js hello world
[ '/usr/local/bin/node', '/path/to/your/index.js', 'hello', 'world' ]
``` 

## 깊이 들어가보기:

커맨드 라인 아규먼트 읽는 방법은 오래된 시스템 프로그래밍에서 시작되었습니다. 이전에는 사용자 입력을 받지 않고 명령어만 실행하는 프로그램들도 있었기 때문에, 사용자의 입력을 받기 위해 커맨드 라인 아규먼트가 사용되었습니다. 다른 대안으로는 환경 변수를 사용하여 정보를 전달하는 방법이 있습니다. 커맨드 라인 아규먼트가 읽혔을 때, 프로그램은 해당 정보를 가지고 더 복잡한 작업을 처리할 수 있습니다. 이 과정은 사용자가 프로그램을 제어하는 방법을 더욱 유연하게 만들어줍니다.

## 관련 레퍼런스: 

- [Node.js Documentation on Process Object](https://nodejs.org/api/process.html#process_process_argv)
- [W3Schools Guide on Command Line Arguments in Javascript](https://www.w3schools.com/js/js_reserved.asp)