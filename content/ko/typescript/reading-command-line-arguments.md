---
title:    "TypeScript: 명령줄 인수 읽기"
keywords: ["TypeScript"]
---

{{< edit_this_page >}}

## 왜

커맨드 라인 인수를 읽는 것이 중요한 이유는 여러 가지가 있습니다. 첫째로, 커맨드 라인 인수는 프로그램의 실행을 제어하는 데 중요한 역할을 합니다. 또한 사용자가 프로그램을 실행할 때 특정한 옵션을 줄 수 있게 해줍니다. 따라서 이러한 이유로 커맨드 라인 인수를 읽는 것은 매우 유용합니다.

## 어떻게

커맨드 라인 인수를 읽는 방법은 여러 가지가 있지만, TypeScript에서는 process 객체를 사용할 수 있습니다. process 객체는 Node.js에서 제공하는 전역 객체로, 프로그램의 실행 환경과 관련된 정보를 제공해줍니다. 따라서 process.argv 속성을 사용하여 커맨드 라인 인수를 읽을 수 있습니다. 아래는 TypeScript로 커맨드 라인 인수를 읽는 간단한 예제와 그에 대한 결과입니다.

```TypeScript
const argsLength: number = process.argv.length;
console.log("Number of arguments: " + argsLength);

// 실행 결과
// Number of arguments: 3
```

위의 예제에서는 process.argv.length 속성을 사용하여 프로그램이 실행될 때 설정된 커맨드 라인 인수의 개수를 확인하고, 이를 콘솔에 출력하고 있습니다. 이외에도 process.argv 배열의 인덱스를 통해 각각의 커맨드 라인 인수에 접근할 수 있습니다.

## 더 깊이 들어가기

커맨드 라인 인수를 읽는 방법에는 더욱 많은 옵션이 있습니다. 예를 들어, minimist라는 라이브러리를 사용하면 커맨드 라인 인수를 더 쉽게 파싱할 수 있습니다. 이 외에도 yargs, commander 등의 라이브러리를 사용하여 커맨드 라인 인수를 처리할 수 있습니다. 하지만 이러한 라이브러리를 사용하지 않고 직접 프로그램을 작성할 때에도 process 객체를 활용하여 커맨드 라인 인수를 읽을 수 있습니다.

## 관련 링크

- [Node.js process 객체 문서](https://nodejs.org/api/process.html)
- [minimist 라이브러리](https://www.npmjs.com/package/minimist)
- [yargs 라이브러리](https://www.npmjs.com/package/yargs)
- [commander 라이브러리](https://www.npmjs.com/package/commander)