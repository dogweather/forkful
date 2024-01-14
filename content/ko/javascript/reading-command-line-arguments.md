---
title:                "Javascript: 컴퓨터 프로그래밍에서 명령 줄 인수 읽기"
simple_title:         "컴퓨터 프로그래밍에서 명령 줄 인수 읽기"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/javascript/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

# 왜

커맨드 라인 인수를 읽는 것은 자바스크립트 프로그래밍에서 중요한 부분입니다. 이를 통해 사용자가 프로그램을 실행할 때 다양한 옵션을 전달할 수 있습니다. 그래서 이를 더 잘 이해하고 활용하기 위해 지금부터 함께해보겠습니다.

## 어떻게

커맨드 라인 인수를 읽기 위해서는 `process.argv`를 사용합니다. 이는 실행되는 파일의 경로와 함께 입력된 인수들을 배열로 반환해줍니다. 아래의 예제를 통해 더 쉽게 이해해봅시다.

```Javascript
console.log(process.argv);
```

위 코드를 실행하면 다음과 같은 결과를 볼 수 있습니다.

```Javascript
[
  '/usr/local/bin/node',
  '/Users/user/programming/index.js',
  'hello',
  'world'
]
```

첫 번째 인수는 Node.js 실행 파일의 경로, 두 번째 인수는 실행되는 파일의 경로이며 나머지 인수들은 사용자가 입력한 인수들입니다.

우리는 이제 `hello`와 `world`를 출력할 수 있도록 코드를 작성해보겠습니다.

```Javascript
const args = process.argv;
console.log(`${args[2]} ${args[3]}`);
```

위 코드를 실행하면 다음과 같은 결과를 볼 수 있습니다.

```
hello world
```

커맨드 라인 인수를 읽는 방법을 알게되었습니다. 이제 더 깊게 들어가보겠습니다.

## 깊이 파고들기

`process.argv`를 통해 우리는 사용자가 입력한 인수들을 얻을 수 있지만, 종종 이들을 더욱 구조화된 데이터로 사용하기를 원할 수 있습니다. 그렇다면 어떻게 해야할까요?

예를 들어, 만약 사용자가 `-u` 옵션을 입력하면 사용자 이름을 `-p` 옵션을 입력하면 비밀번호를 받는 프로그램을 만든다고 생각해보겠습니다. 이 경우 우리는 `process.argv`를 더욱 유연하게 다루는 방법이 필요합니다.

여러분은 `yargs`라는 모듈을 사용할 수 있습니다. 이 모듈을 사용하면 매우 쉽게 커맨드 라인 인수를 다룰 수 있습니다. 먼저 `yargs`를 설치하겠습니다.

```
npm install yargs
```

그리고 다음과 같이 코드를 작성해봅시다.

```Javascript
const argv = require('yargs').argv;
console.log(`Username: ${argv.u}, Password: ${argv.p}`);
```

여기서 `u`와 `p`는 우리가 원하는 이름으로 변경할 수 있습니다. 이제 사용자가 `-u Bob -p abc123`와 같이 입력하는 경우를 생각해보겠습니다. 우리는 다음과 같은 결과를 볼 수 있습니다.

```
Username: Bob, Password: abc123
```

`yargs`를 사용하면 더욱 편리하게 커맨드 라인 인수를 다룰 수 있으며 추가적인 기능들도 제공해줍니다.

## 참고하기

- [Node.js documentation on Process](https://nodejs.org/docs/latest/api/process.html#process_process_argv)
- [yargs documentation](https://www.npmjs.com/package/yargs)