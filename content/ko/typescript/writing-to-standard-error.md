---
title:                "표준 에러에 쓰는 방법"
html_title:           "TypeScript: 표준 에러에 쓰는 방법"
simple_title:         "표준 에러에 쓰는 방법"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/typescript/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## 무엇 & 왜?
표준 오류에 쓰기란 무엇인가요? 그리고 프로그래머가 왜 이렇게 하는 걸까요? 
표준 오류에 쓰기는 프로그램의 실행 중 발생하는 오류 정보를 출력하는 것을 의미합니다. 프로그램 실행 중 오류가 발생하면 프로그램은 종료되는 경우가 많은데, 이때 종료된 이유를 파악하기 위해 오류 정보를 출력할 수 있습니다. 따라서 개발자는 어떤 오류가 발생했는지 빠르게 파악할 수 있으며, 이를 통해 더 나은 디버깅과 개발이 가능합니다. 

## 사용 방법:
아래는 TypeScript에서 표준 오류에 쓰는 방법의 예시 코드와 결과물입니다.

```TypeScript
console.error("Error: Invalid input.");
```
결과:
```
Error: Invalid input.
```

## 깊이 들어가기:
### 역사적 배경:
표준 오류에 쓰기는 1970년대의 유닉스 운영체제에서 처음 사용되었습니다. 당시에는 오류 메시지를 출력하여 디버깅을 돕기 위한 목적으로 사용되었습니다. 따라서 현재까지도 프로그래밍에서 널리 사용되는 기능입니다.

### 다른 대안들:
표준 오류에 쓰는 다른 대안으로는 `console.log()` 함수가 있습니다. 이 함수는 오류 메시지뿐만 아니라 일반적인 메시지를 출력할 때도 사용될 수 있습니다. 그러나 `console.log()` 함수는 표준 출력에 쓰기 때문에 오류 메시지를 따로 구분하기 어렵다는 단점이 있습니다.

### 구현 세부사항:
일반적으로 프로그램에서 오류가 발생할 때는 `throw` 구문을 사용하여 오류를 던지게 됩니다. 이때 `Error()` 객체를 사용하여 오류 메시지를 생성하고, `console.error()` 함수를 통해 오류 메시지를 출력할 수 있습니다.

## 관련 자료:
- [MDN: console.error()](https://developer.mozilla.org/ko/docs/Web/API/Console/error)
- [MDN: Error](https://developer.mozilla.org/ko/docs/Web/JavaScript/Reference/Global_Objects/Error)