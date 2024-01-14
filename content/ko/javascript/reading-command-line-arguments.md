---
title:    "Javascript: 컴퓨터 프로그래밍에서 명령 줄 인수 읽기"
keywords: ["Javascript"]
---

{{< edit_this_page >}}

## 왜

누군가 커맨드 라인 아규먼트를 읽는 것에 참여하는 이유는 코드를 사용하여 효과적으로 프로그래밍을 할 수 있기 때문입니다.

## 어떻게

커맨드 라인 아규먼트는 프로그래밍에서 매우 유용한 기능입니다. 이 기능을 사용하여 다양한 환경에서 동적으로 프로그램을 실행할 수 있습니다. 다음은 커맨드 라인 아규먼트를 읽고 출력하는 간단한 Javascript 코드 예제입니다.

```Javascript
// 프로그램 실행 시 커맨드 라인에서 두 개의 값 입력 받기
const arg1 = process.argv[2];
const arg2 = process.argv[3];

// 입력 받은 값 출력
console.log("첫 번째 값 : " + arg1);
console.log("두 번째 값 : " + arg2);
```

위의 코드를 실행하면 커맨드 라인에서 입력된 두 개의 값이 각각 arg1과 arg2 변수에 할당되고, 이후에는 해당 값을 출력하는 결과를 볼 수 있습니다.

```
> node program.js value1 value2
첫 번째 값 : value1
두 번째 값 : value2
```

이와 같이 커맨드 라인 아규먼트를 읽는 것은 프로그램의 실행 시 동적으로 값을 입력받아 원하는 작업을 수행하는 데에 매우 유용합니다. 이 기능을 이용하면 사용자 입력을 받거나 설정 파일을 읽는 대신 코드 내부에서 값을 할당하는 등 다양한 활용이 가능합니다.

## 깊이 파고들기

커맨드 라인 아규먼트를 읽는 방법에는 여러 가지가 있지만, 가장 일반적인 방법은 process 객체의 argv 속성을 사용하는 것입니다. 이 속성은 프로그램 실행 시 커맨드 라인에서 입력된 모든 값을 배열로 저장하고 있습니다. 이 배열의 첫 번째 요소는 항상 node의 실행 경로, 두 번째 요소는 실행한 파일의 경로를 나타내며, 세 번째 요소부터가 입력된 값들이 순서대로 저장되어 있습니다.

또한 process 객체는 argv 이외에도 다양한 속성을 제공하고 있으므로, 이를 통해 더 다양한 정보를 읽고 활용할 수 있습니다. 좀 더 자세한 내용은 공식 문서를 참고하시기 바랍니다.

## 참고 자료

- [Node.js 공식 문서 - Process](https://nodejs.org/api/process.html#process_process_argv)
- [Node.js로 CLI(Command Line Interface) 프로그램 만들기](https://www.zerocho.com/category/NodeJS/post/577bffca37b9420001dccd0f)
- [Taking command line arguments with Node.js](https://stackabuse.com/taking-command-line-arguments-with-node-js/)