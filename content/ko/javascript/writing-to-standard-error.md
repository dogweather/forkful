---
title:                "Javascript: 표준 오류에 쓰는 방법"
programming_language: "Javascript"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/javascript/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## 왜

표준 오류에 쓰는 일에 대해 궁금해하는 분들에게 이 포스팅을 써보려고 합니다.

## 어떻게

보통 자바스크립트에서 콘솔에 출력할 때는 `console.log()`를 사용하게 됩니다. 하지만 때로는 콘솔이 아닌 다른 곳에서 에러 메시지나 로그를 출력해야 할 때가 있습니다. 이때 사용하는 것이 바로 `process.stderr.write()`입니다. 아래는 코드 예시와 함께 출력되는 결과입니다.

```Javascript
process.stderr.write("에러가 발생했습니다.");
```

```
에러가 발생했습니다.
```

## 딥 다이브

`process.stderr.write()`는 `console.log()`와는 다르게 바로 콘솔에 출력되는 게 아니라, 버퍼에 기록된 다음에 한꺼번에 출력되는 것입니다. 이를 통해 성능을 개선할 수 있고, 아주 긴 출력 메시지도 일괄 처리할 수 있습니다.

## 참고 자료

- [Node.js 공식 문서 - process.stderr.write()](https://nodejs.org/api/process.html#process_process_stderr_write_data_encoding_callback)
- [Error Handling in Node.js](https://www.sitepoint.com/error-handling-in-node-js/)
- [Node.js의 기본 개념과 개요](https://nodejs.dev/learn)
- [Node.js 에러 핸들링 기초](https://www.freecodecamp.org/news/node-js-error-handling/)

## 참고하실 링크

(process.stderr.write() 사용법 번역 링크 여기) (딥 다이브에 대한 따로 번역된 내용 찾아 추가하면 좋을 것 같습니다)