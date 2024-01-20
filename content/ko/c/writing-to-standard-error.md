---
title:                "표준 에러 작성하기"
html_title:           "C: 표준 에러 작성하기"
simple_title:         "표준 에러 작성하기"
programming_language: "C"
category:             "C"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/c/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## 무엇 & 왜?

표준 에러 로그에 쓰기란 무엇을 의미하는가? 이것은 프로그래머가 컴퓨터 프로그램에서 나타는 오류 또는 경고 메시지를 기록하는 방법입니다. 이렇게 하면 프로그램이 실행 중에 문제가 발생하면 쉽게 찾아 수정할 수 있습니다.

## 어떻게:

```c
#include <stdio.h>

int main()
{
  fprintf(stderr, "Error: division by zero");
  return 0;
}
```
위의 예제에서 ``` fprintf(stderr, "Error: division by zero"); ``` 는 표준 에러 로그에 "Error: division by zero" 메시지를 작성하는 방법을 보여줍니다. 만약 이 코드가 실행 중에 오류가 발생하면, 해당 메시지가 표시됩니다.

## 깊이 들어가기:

### 역사적 맥락:

표준 에러 로그에 쓰기는 오래된 프로그래밍 기술입니다. 이전에는 오류 메시지를 모니터에 직접 출력하는 것이 일반적이었습니다. 그러나 이는 사용자가 보는 것이 아니기 때문에, 프로그래머에게 오류 정보를 전달하는 데 어려움이 있었습니다. 그래서 표준 에러 로그에 쓰기가 만들어졌습니다.

### 대안:

표준 에러 로그에 쓰기는 오류 정보를 기록하는 가장 일반적인 방법 중 하나입니다. 그러나 다른 방법으로는 파일, 데이터베이스, 또는 로그 파일에 오류를 기록하는 것이 있습니다.

### 구현 세부 정보:

표준 에러 로그에 쓰는 방법은 각 프로그래밍 언어마다 다릅니다. C 프로그래밍 언어에서는 ``` fprintf ``` 함수를 사용하고, Java에서는 ``` System.err.println() ``` 메소드를 사용합니다. 또한, 프로그래밍 언어 외에도 운영 체제에 따라 출력되는 방식이 다를 수 있습니다.

## 관련 자료:

- [Java 프로그래밍 언어 공식 문서](https://docs.oracle.com/javase/specs/jls/se8/html/jls-8.html#jls-8.4)
- [생각보다 중요한 표준 에러 로그에 대해 알아보기](https://medium.com/@rajatmehra0506/the-one-thing-you-should-really-know-about-logging-tools-standard-error-9e6d4c27e1f3)