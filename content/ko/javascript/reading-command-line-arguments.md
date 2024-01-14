---
title:    "Javascript: 컴퓨터 프로그래밍: 명령 줄 인수 읽기"
keywords: ["Javascript"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/ko/javascript/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

왜: 커맨드 라인 인수를 읽는 것에 대해 읽기를 하는 이유는 무엇인가요? 단 1-2 문장으로 설명합니다.

커맨드 라인 인수는 프로그래밍에서 자주 사용되는 중요한 개념입니다. 프로그램을 실행할 때 커맨드 라인에서 인수를 입력하여 프로그램의 동작을 제어할 수 있습니다. 따라서 커맨드 라인 인수를 읽는 방법을 이해하는 것은 매우 중요합니다. 이 블로그 포스트에서는 Javascript를 사용하여 커맨드 라인 인수를 읽는 방법에 대해 배워보겠습니다.

## 어떻게

커맨드 라인 인수를 읽기 위해서는 ```process.argv```라고 불리는 빌트인 노드 js 객체를 사용합니다. 이 객체는 현재 실행 중인 프로그램에 전달된 인수들을 배열로 저장합니다. 예를 들어, 다음과 같은 커맨드 라인을 입력하면:

```
node index.js hello world
```

다음과 같은 배열로 접근할 수 있습니다:

```Javascript
const args = process.argv;
console.log(args);
```

```
["node", "index.js", "hello", "world"]
```

첫 번째 요소는 노드 자체의 경로이며 두 번째는 현재 실행 중인 파일의 경로입니다. 따라서 실제 인수들은 배열의 세 번째 요소부터 시작하게 됩니다. 이를 이용하여 우리는 다음과 같이 해당 인수들에 접근할 수 있습니다:

```Javascript
const arg1 = args[2];
const arg2 = args[3];
```

결과적으로, 위의 예제에서 arg1은 "hello"가 되고 arg2는 "world"가 됩니다. 이제 우리는 이 인수들을 원하는 방식으로 활용할 수 있습니다.

## 더 깊이 들어가기

지금까지 우리는 간단하게 커맨드 라인 인수를 읽는 방법을 살펴보았습니다. 하지만 이 방법은 모든 상황에서 적용되는 것은 아닙니다. 특정 옵션이 들어올 경우 다른 동작을 하도록 하고 싶은 경우에는 어떻게 해야 할까요? 이러한 경우에는 ```commander```라고 불리는 라이브러리를 사용하면 쉽게 해결할 수 있습니다. 이 라이브러리는 기능이 다양하고 명령어 구문을 파싱해주는 기능을 제공합니다. 우리는 이를 사용하여 커맨드 라인 인수를 편리하게 처리할 수 있습니다.

## 더보기

- [Node.js 공식 문서 - Process 모듈](https://nodejs.org/dist/latest-v14.x/docs/api/process.html#process_process_argv)
- [Commander 라이브러리 문서](https://www.npmjs.com/package/commander)