---
title:                "TypeScript: 디버그 출력을 출력하기"
programming_language: "TypeScript"
category:             "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/typescript/printing-debug-output.md"
---

{{< edit_this_page >}}

## 왜 해야 할까?

디버그 출력을 활용하는 이유는 간단합니다. 만약 당신이 프로그래머라면, 코드를 디버그하고 문제를 해결하는 것이 가장 중요한 일 중 하나일 것입니다. 시스템에서 무엇이 잘못됐는지 확인하는 가장 쉬운 방법 중 하나가 바로 디버그 출력을 통해 확인하는 것입니다. 이를 통해 더 나은 디버깅 경험을 만들 수 있습니다.

## 어떻게 해야 할까?

디버그 출력을 활용하는 방법은 간단합니다. 먼저 ```console.log()``` 함수를 사용하여 출력하고자 하는 값을 지정할 수 있습니다. 예를 들어, 다음의 코드를 확인해 보세요.

```TypeScript
let number = 5;
console.log("Number is:", number);
```

위 코드는 콘솔에 ```Number is: 5```를 출력합니다. 이처럼 우리는 콘솔에 변수의 값을 출력함으로써 디버깅을 할 수 있습니다. 또한 ```console.error()``` 함수를 사용하여 에러 메시지를 출력할 수도 있습니다.

## 깊이 들어가기

디버그 출력은 디버깅을 위해 가장 많이 사용되는 방법입니다. 하지만 디버그 출력은 우리가 잘못된 곳에서 값을 출력하고 있는지, 값의 타입이 맞는지 등 자세한 정보를 제공하지는 않습니다. 이럴 때는 ```console.table()``` 함수를 사용해야 합니다. 이 함수는 객체를 테이블 형태로 콘솔에 출력합니다. 예를 들어, 다음의 코드를 확인해 보세요.

```TypeScript
let person = {
  name: "John",
  age: 25
};

console.table(person);
```

위 코드는 콘솔에 객체 ```person```을 테이블 형태로 출력합니다. 이렇게 하면 더 많은 정보를 제공받을 수 있어 디버깅이 더 쉬워집니다.

## 보기

디버그 출력이 얼마나 유용한지 알아보았습니다. 하지만 디버그 출력만으로 모든 문제를 해결할 수는 없습니다. 그래서 아래의 링크들을 참고하여 디버깅 경험을 더욱 향상시킬 수 있도록 노력해 보세요.

### 참고 링크들

- [MDN web docs - Console API](https://developer.mozilla.org/en-US/docs/Web/API/Console)
- [Microsoft TypeScript handbook - Debugging](https://www.typescriptlang.org/docs/handbook/debugging.html)
- [LogRocket - Debugging TypeScript code with logging and the debugger statement](https://blog.logrocket.com/debug-typescript-code/)