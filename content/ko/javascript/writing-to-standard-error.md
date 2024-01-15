---
title:                "표준 오류에 쓰는 방법"
html_title:           "Javascript: 표준 오류에 쓰는 방법"
simple_title:         "표준 오류에 쓰는 방법"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/javascript/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## 왜?

Javascript 프로그래밍에서는 종종 표준 오류에 대한 정보를 출력해야 할 때가 있습니다. 이를 활용하면 디버깅이나 예외 처리 등에 유용한 정보를 얻을 수 있습니다.

## 어떻게?

표준 오류에 정보를 출력하는 기본적인 방법은 `console.error()` 함수를 사용하는 것입니다. 이 함수에 문자열, 변수, 객체 등을 인자로 넣으면 해당 값들이 표준 오류에 출력됩니다.

```Javascript
let num = 10;
console.error("Number:", num);

// Output:
// Number: 10
```

표준 오류에 출력되는 정보가 너무 많을 경우, `console.group()`과 `console.groupEnd()`를 사용하여 정보를 그룹화할 수 있습니다. 이를 활용하면 출력되는 정보의 구조가 더욱 명확해지고 디버깅을 더 쉽게 할 수 있습니다.

```Javascript
console.group("Info");
console.error("Name: John");
console.error("Age: 30");
console.groupEnd();

// Output:
// Info
// Name: John
// Age: 30
```

## 심층 분석

표준 오류를 출력하는 것만으로는 디버깅에는 부족할 수 있습니다. 따라서 `console.trace()` 함수를 사용하여 오류가 발생한 위치를 추적할 수 있습니다. 이 함수를 통해 스택 트레이스를 출력하면 오류가 발생한 함수의 호출 경로를 확인할 수 있습니다. 이를 활용하여 디버깅을 더욱 쉽게 할 수 있습니다.

```Javascript
function add(a, b) {
  return a + b;
}

function multiply(a, b) {
  return a * b;
}

console.trace("Error");
console.error("Result:", add(2, multiply(3, 4)));
console.trace("End");

// Output:
// Error
// Result: 14
//     at add (repl:1:12)
//     at repl:1:1
//     at Script.runInThisContext (vm.js:130:18)
//     at REPLServer.defaultEval (repl.js:476:29)
//     at bound (domain.js:422:15)
//     at REPLServer.runBound [as eval] (domain.js:435:12)
//     at REPLServer.onLine (repl.js:832:10)
//     at REPLServer.emit (events.js:315:20)
//     at REPLServer.EventEmitter.emit (domain.js:482:12)
//     at REPLServer.Interface._onLine (readline.js:416:10)
// End
```

## 또 보기

- [Javascript의 `console` 객체](https://developer.mozilla.org/ko/docs/Web/API/Console)
- [오류 추적과 디버깅](https://poiemaweb.com/js-error)