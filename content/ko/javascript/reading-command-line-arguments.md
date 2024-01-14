---
title:                "Javascript: 컴퓨터 프로그래밍에서의 명령 줄 인수 읽기"
programming_language: "Javascript"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/javascript/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## 왜

자바스크립트 프로그래밍을 공부하고 있거나 개발을 하고 있는 사람들은 때때로 커맨드 라인 인자를 읽어야 할 필요가 있습니다. 이를 통해 프로그램이 실행될 때 사용자가 입력한 인자를 읽을 수 있고, 이를 기반으로 프로그램의 동작을 제어할 수 있습니다. 이 글에서는 자바스크립트에서 커맨드 라인 인자를 읽는 방법을 알려드리겠습니다.

## 하우 투

사용자가 입력한 커맨드 라인 인자를 읽기 위해 우리는 process 객체의 argv 속성을 사용합니다. 이 속성은 문자열 배열로 이루어져 있으며, 프로그램을 실행할 때 입력한 모든 인자를 담고 있습니다. 예를 들어, 다음과 같이 프로그램을 실행했을 때:

```
node index.js hello world
```

우리는 hello와 world라는 두 개의 인자를 받게 됩니다. 아래는 이를 확인하기 위한 간단한 예제 코드입니다.

```Javascript
// index.js
console.log(process.argv[2]); // hello
console.log(process.argv[3]); // world
```

위 코드를 실행하면 hello와 world가 순서대로 출력됩니다. 또 다른 예제로, 사용자에게 이름을 입력받아 인사하는 간단한 프로그램을 만들어 보겠습니다.

```Javascript
// index.js
const name = process.argv[2];
console.log(`Hello ${name}!`);
```

위 코드를 실행하면 다음과 같이 사용자가 입력한 이름에 따라 다른 인삿말이 출력됩니다.

```
node index.js John
// Hello John!
```

## 딥 다이브

위에서 간단하게 살펴본 것처럼, 자바스크립트에서 커맨드 라인 인자를 읽는 것은 매우 간단합니다. 하지만 더 깊게 파고들면 더 다양한 기능을 사용할 수 있습니다. 예를 들어, minimist와 같은 패키지를 사용하면 인자의 이름과 값을 쌍으로 연결하여 더 편리하게 사용할 수 있습니다. 또한, 프로그램을 실행할 때 옵션을 설정하거나 도움말을 출력하는 등 강력한 기능들을 제공하는 패키지도 존재합니다. 이를 사용해 보면 자바스크립트에서도 보다 다양한 커맨드 라인 인자 처리를 할 수 있습니다.

## 봐도 좋아요

- [Node.js Documentations](https://nodejs.org/api/process.html#process_process_argv)
- [minimist Package](https://www.npmjs.com/package/minimist)
- [Commander Package](https://www.npmjs.com/package/commander)
- [yargs Package](https://www.npmjs.com/package/yargs)