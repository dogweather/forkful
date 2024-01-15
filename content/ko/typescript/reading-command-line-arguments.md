---
title:                "컴퓨터 프로그래밍의 기사 제목 : 명령 줄 인수 읽기"
html_title:           "TypeScript: 컴퓨터 프로그래밍의 기사 제목 : 명령 줄 인수 읽기"
simple_title:         "컴퓨터 프로그래밍의 기사 제목 : 명령 줄 인수 읽기"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/typescript/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

# 왜

## 왜 TypeScript로 커맨드 라인 인수를 읽어야 할까요?

커맨드 라인 인수를 읽는 것은 많은 개발에 필수적입니다. 프로그램에 입력을 제공하고 원하는 작업을 수행할 수 있도록 도와주는 것입니다. TypeScript를 사용하는 이유는 다음과 같습니다. 첫째, TypeScript는 자바스크립트의 일부를 포함하여 강력한 정적 타입 검사를 제공하므로 코드를 더욱 안전하고 예측 가능하게 만듭니다. 둘째, TypeScript는 모듈 시스템을 지원하여 코드의 구조와 유지 보수를 쉽게 만듭니다. 세번째, TypeScript는 많은 개발자들이 즐겨 사용하는 현대적인 언어이기 때문에 자바스크립트를 공부하고 익히는 데 많은 시간을 절약할 수 있습니다.

# 어떻게

## 커맨드 라인 인수를 TypeScript로 읽는 방법

커맨드 라인 인수를 읽는 것은 매우 간단한 작업입니다. TypeScript에서 외부 라이브러리나 모듈 없이도 커맨드 라인 인수를 읽을 수 있습니다.

## 예제 코드

```TypeScript
// process 객체에서 제공하는 argv 배열을 사용하여 커맨드 라인 인수를 읽을 수 있습니다.
const args = process.argv;
// argv 배열의 첫번째 요소는 node 실행 파일의 경로이므로 제외하고 두번째 요소부터 실제 인수가 시작됩니다.
// slice 함수를 사용하여 argv 배열에서 두번째 요소부터 끝까지를 새로운 배열로 만듭니다.
const inputArgs = args.slice(2);
console.log(inputArgs); // 입력한 커맨드 라인 인수 출력
```

### 예제 실행

코드를 실행할 때 입력한 커맨드 라인 인수가 출력됩니다. 아래 예제는 "node index.ts arg1 arg2 arg3" 명령어를 입력한 후의 결과값입니다.

![예제 실행 결과 이미지](./example-output.png)

또 다른 방법으로는 "minimist"라는 외부 라이브러리를 사용하는 것입니다. 이 라이브러리는 입력된 커맨드 라인 인수를 파싱하여 쉽게 사용할 수 있는 객체로 만들어 줍니다.

```TypeScript
import * as minimist from "minimist";

// minimist를 사용하여 입력된 커맨드 라인 인수를 파싱합니다.
const args = minimist(process.argv.slice(2));
// args 객체에서 원하는 인수를 가져와 사용합니다.
console.log(args.arg1);
console.log(args.arg2);
console.log(args.arg3);
```

### 예제 실행

위 예제 코드도 같은 명령어를 입력한 후 아래와 같은 결과값을 출력합니다.

![예제 실행 결과 이미지](./example-output-2.png)

# 딥 다이브

## 커맨드 라인 인수 읽기의 원리

커맨드 라인 인수를 읽는 원리는 매우 간단합니다. 우선 node 실행 파일의 경로와 실제 인수들이 담긴 문자열 배열이 process 객체에서 제공되는 argv 변수에 할당됩니다. 그리고 우리는 argv 배열에서 필요한 부분만 잘라내어 사용하면 됩니다.

외부 라이브러리를 사용하는 경우에는 해당 라이브러리의 기능에 따라 인수들이 어떻게 파싱되고 처리되는지를 알아야 합니다.

## 자세한 정보