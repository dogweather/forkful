---
title:                "TypeScript: 컴퓨터 프로그래밍을 위한 커맨드 라인 인수 읽기"
programming_language: "TypeScript"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/typescript/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

# 왜

타입스크립트 프로그래밍을 할 때 사용자의 입력 값을 읽어들이는 것은 매우 중요합니다. 왜냐하면 이것은 사용자와의 상호작용을 통해 프로그램의 동작을 제어하기 위해 필요하기 때문입니다. 예를 들어, 사용자로부터 입력 받은 값을 기반으로 프로그램이 다른 동작을 수행할 수 있습니다. 

# 어떻게

사용자의 입력 값을 읽어들이는 가장 간단한 방법은 커맨드 라인 인자를 사용하는 것입니다. 타입스크립트에서 커맨드 라인 인자를 읽는 방법은 아래와 같이 코드로 구현할 수 있습니다.

```TypeScript
const args = process.argv;
console.log(args); 
```

위 코드는 `process` 객체의 `argv` 프로퍼티를 통해 실행 시 제공되는 커맨드 라인 인자를 읽어들입니다. 그리고 `console.log()` 함수를 통해 해당 인자를 출력하게 됩니다. 

예를 들어, 다음과 같이 `node` 명령어를 사용하여 위 코드를 실행할 수 있습니다.

```bash
node index.ts Hello World
```

위 코드는 `Hello`와 `World`라는 두 개의 커맨드 라인 인자를 사용하여 `index.ts` 파일을 실행하게 됩니다. 그리고 아래와 같은 결과를 출력하게 됩니다.

```bash
["node", "index.ts", "Hello", "World"]
```

# 깊이 파고들기

커맨드 라인 인자를 읽는 것은 매우 간단해 보입니다. 하지만 실제로는 더 복잡한 작업을 수행할 수 있습니다. 예를 들어, `yargs`라는 패키지를 사용하면 더 편리하고 사용하기 쉬운 인터페이스를 제공해줍니다. 이 패키지를 사용하면 커맨드 라인 인자를 자동으로 파싱하여 객체 형태로 제공해줍니다. 

또한, 커맨드 라인 인자를 사용하여 조건문을 활용할 수도 있습니다. 예를 들어, 아래와 같은 코드를 작성하면 커맨드 라인 인자에 따라 다른 동작을 수행할 수 있습니다.

```TypeScript
const args = process.argv;

if (args[2] === "Hello") {
    console.log("Hello, world!");
} else if (args[2] === "Goodbye") {
    console.log("Goodbye, world!");
}
```

위 코드에서는 인자로 `Hello`가 전달되면 `Hello, world!`라는 문자열을 출력하고, `Goodbye`가 전달되면 `Goodbye, world!`라는 문자열을 출력합니다.

# 보충 자료

- [Node.js - 커맨드 라인 인자 읽기](https://www.tutorialspoint.com/nodejs/nodejs_command_line_arguments.htm)
- [Yargs - GitHub 저장소](https://github.com/yargs/yargs)