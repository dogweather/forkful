---
title:                "TypeScript: 표준 에러에 쓰는 방법"
programming_language: "TypeScript"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/typescript/writing-to-standard-error.md"
---

{{< edit_this_page >}}

버그와 오류는 프로그래밍에서 피할 수 없는 부품입니다. 하지만 오류를 발견하고 고치기 위해서는 적절한 방법으로 결과물을 추적해야 합니다. 이때 표준 에러 스트림에 적절한 메세지를 출력하는 것은 오류를 찾고 수정하는데 큰 도움이 됩니다. 이번 포스팅에서는 TypeScript를 사용하여 표준 에러 스트림에 메세지를 출력하는 방법을 알아보겠습니다.

## 왜

표준 에러 스트림을 사용하여 메세지를 출력하는 가장 큰 이유는 디버깅과 오류 수정입니다. 오류가 발생하면 프로그램이 멈추고 에러 메세지가 터미널에 출력됩니다. 하지만 이 메세지가 어디서 왔는지 정확하게 알기 위해서는 해당 메세지를 표준 에러 스트림에 출력하는 것이 중요합니다. 이를 통해서 오류 발생 위치를 찾고 고칠 수 있습니다.

## 방법

우선 표준 에러 스트림을 출력하기 위해서는 `console.error()` 함수를 사용합니다. 이 함수는 인자로 받은 메세지를 표준 에러 스트림에 출력합니다. 아래 예제 코드를 살펴보겠습니다.

```TypeScript
let num: string = "10";
console.error("Number is: " + num);
```

위 코드를 실행하면 터미널에 "Number is: 10"이라는 메세지가 출력됩니다. 이는 표준 에러 스트림에 출력되는 것을 확인할 수 있습니다.

## 자세히 보기

표준 에러 스트림은 표준 출력 스트림과 달리 오류 메세지만을 출력하는 역할을 합니다. 때문에 각각의 스트림에 출력할 때 순서를 지켜주는 것이 중요합니다. 또한, 표준 에러 스트림에서 출력한 메세지는 파일로 리디렉션할 수 있기 때문에 이를 활용하면 오류를 기록하는 로그를 만들 수도 있습니다.

## 참고자료

[TypeScript 공식 문서](https://www.typescriptlang.org/docs/handbook/release-notes/typescript-2-3.html)

[표준 출력과 표준 에러 스트림의 차이](https://stackoverflow.com/questions/2342826/how-to-distinguish-stdout-and-stderr-in-bash)

See Also:

[TypeScript 디버깅 가이드](https://www.typescriptlang.org/docs/handbook/debugging.html)

[표준 에러 스트림 리디렉션 예제](https://www.geeksforgeeks.org/io-redirection-in-linux/)