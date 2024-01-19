---
title:                "디버그 출력을 인쇄하기"
html_title:           "Clojure: 디버그 출력을 인쇄하기"
simple_title:         "디버그 출력을 인쇄하기"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/javascript/printing-debug-output.md"
---

{{< edit_this_page >}}

## 무엇 & 왜?

디버그 출력은 프로그램이 일련의 명령을 어떻게 실행하는지 확인하기 위해 사용하는 메시지입니다. 이는 프로그래머가 코드 내에 있는 문제를 찾고 수정하는 데 도움이 됩니다.

## 사용법:

```Javascript
console.log("디버그 메시지입니다."); 
```
위의 샘플 코드를 실행하면, 출력은 다음과 같습니다.

```Javascript
"디버그 메시지입니다."
```
변수의 값을 출력하려면:

```Javascript
let x = 5;
console.log("x의 값은 " + x);
```
이 경우, 출력은 다음과 같습니다.

```Javascript
"x의 값은 5"
```

## 딥 다이브

디버그 출력의 기능은 `console.log`와 같은 함수를 통해 구현되며, 초기 프로그래밍 언어인 Fortran과 같은 언어에서 사용하던 출력 문장의 발전입니다. 

디버그 출력 외에도 브레이크 포인트 사용, 단위 테스트 등 다른 디버깅 기법도 있습니다. 

`console.log`의 구현은 대부분의 브라우저 및 노드.js에서 제공하는 기본 자바스크립트 API를 통해 이루어집니다. 그러나 세부 실행 방식은 브라우저 종류나 노드.js 버전에 따라 조금씩 다를 수 있습니다.

## 참고 자료:

1. [MDN console.log() 설명문서](https://developer.mozilla.org/ko/docs/Web/API/Console/log)
2. [자바스크립트 디버깅 기법에 대한 개요](https://www.w3schools.com/js/js_debugging.asp)
3. [노드JS 문서](https://nodejs.org/en/docs/guides/debugging-getting-started/)