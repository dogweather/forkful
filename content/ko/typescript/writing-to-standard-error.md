---
title:    "TypeScript: 표준 에러에 쓰는 방법"
keywords: ["TypeScript"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/ko/typescript/writing-to-standard-error.md"
---

{{< edit_this_page >}}

# 왜
기본적으로 TypeScript로 개발을 할 때, 많은 이유로 코드를 디버깅해야 합니다. 그리고 때때로 특정 코드가 에러를 발생시킬 수 있습니다. 이때 표준 에러를 활용하면 쉽게 에러를 파악하고 디버깅 할 수 있습니다.

## 어떻게 할까요?
가장 기본적인 방법은 `console.error()` 함수를 사용하는 것입니다. 이 함수는 에러를 화면에 출력하는 것이 아니라 표준 에러 스트림에 로그를 기록합니다. 이를 활용하여 코드에서 발생하는 에러를 신속하게 감지할 수 있습니다.

```TypeScript
console.error("이것은 에러 메시지입니다.");
```

위 코드를 실행하면, 콘솔에서 아래와 같이 에러 메시지가 출력됩니다.

```
이것은 에러 메시지입니다.
```

## 깊이 파고들기
`console.error` 함수를 사용하는 것 이외에, TypeScript에서 표준 에러를 다루는 방법에는 몇 가지 더 있습니다. 가장 많이 사용되는 방법은 `process.on` 메소드를 활용하는 것입니다. 이 메소드를 사용하면 예외가 발생했을 때 코드를 작성해 신중하게 에러를 다룰 수 있습니다. 아래는 `process.on` 메소드를 사용하는 예제 코드입니다.

```TypeScript
import { write } from "fs";

process.on("uncaughtException", function(error) {
    // 발생한 예외를 표준 에러에 쓰기
    write(STDERR_FILENO, error.stack + "\n");
});
```

위 예제 코드를 실행하면, `uncaughtException` 콜백 함수가 호출되고 발생한 예외를 표준 에러에 쓰게 됩니다.

# 관련 자료
- [Node.js 공식 문서 | 표준 스트림](https://nodejs.org/api/process.html#process_process_stdio)
- [TypeScript 공식 문서 | 컴파일러 옵션](https://www.typescriptlang.org/docs/handbook/compiler-options.html)