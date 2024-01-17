---
title:                "컴퓨터 프로그래밍: 명령줄 인수 읽기"
html_title:           "TypeScript: 컴퓨터 프로그래밍: 명령줄 인수 읽기"
simple_title:         "컴퓨터 프로그래밍: 명령줄 인수 읽기"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/typescript/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

# What & Why?
커맨드 라인 인수를 읽는 것은 프로그래머들이 사용되는 인수를 확인하고 이를 다른 코드 또는 클라이언트에 전달하기 위한 방법입니다. 커맨드 라인 인수를 읽는 것은 사용자 지정 옵션을 제공하거나 런타임에 동작을 변경하고자 할 때 유용합니다.

# How to:
커맨드 라인 인수를 읽는 가장 간단한 방법은 `process.argv`를 사용하는 것입니다. 이를 통해 프로그램이 실행되는 환경에서 전달된 인수의 배열을 얻을 수 있습니다. 예를 들어, 다음 코드는 `--name` 옵션과 함께 실행되는 경우 `Name: John Doe`라는 문자열을 출력합니다.

```TypeScript
const args = process.argv;

if (args.includes('--name')) {
  const nameIndex = args.indexOf('--name');
  const name = args[nameIndex + 1];
  console.log(`Name: ${name}`);
}
```

```
$ node index.js --name "John Doe"

Name: John Doe
```

# Deep Dive:
커맨드 라인 인수를 다루는 것은 오래된 전통입니다. 초기 컴퓨터 시스템에서는 커맨드 라인 인수가 다양한 환경 변수와 함께 사용되었으며, 이는 사용자가 프로그램을 실행할 때 옵션을 전달할 수 있는 유연성을 제공했습니다. 현대에는 더 강력한 옵션 파서 라이브러리가 있지만 간단한 프로그램에서는 `process.argv`를 사용하는 것이 더 편리할 수 있습니다. 또한, 다중 커맨드로의 확장을 고려하여 커맨드 라인 인수를 분석하는 라이브러리를 사용하는 것이 더 안전합니다. 

# See Also:
- [Node.js API: Process](https://nodejs.org/api/process.html)
- [Commander.js - Command-line interfaces made easy](https://github.com/tj/commander.js)