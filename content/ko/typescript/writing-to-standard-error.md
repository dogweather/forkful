---
title:                "표준 오류로 쓰기"
html_title:           "Bash: 표준 오류로 쓰기"
simple_title:         "표준 오류로 쓰기"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Files and I/O"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/typescript/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## What & Why? (무엇이며 왜?)
표준 오류는 에러 메시지를 출력하기 위한 특별한 데이터 스트림입니다. 프로그래머들이 이를 사용하는 이유는 에러 메시지를 일반 출력과 구분해 운영체제나 다른 프로그램이 효과적으로 처리할 수 있게 하기 위해서입니다.

## How to: (방법)
```TypeScript
console.error('에러 발생: 파일을 읽을 수 없음.');

// 혹은 당신이 Error 객체를 사용하는 경우
const error = new Error('실패: 데이터베이스 연결 불가.');
console.error(error);
```
출력 예시:
```
에러 발생: 파일을 읽을 수 없음.
Error: 실패: 데이터베이스 연결 불가.
```

## Deep Dive (심층 분석)
초기 유닉스 시스템에서 표준 오류는 프로그램의 오류 메시지를 분리하여 처리하기 위해 도입되었습니다. 이는 프로그램이나 스크립트가 표준 출력(stdout)을 다른 목적으로 사용하더라도 오류 추적을 가능하게 했습니다. 대안으로는 파일 로깅이나 원격 로깅 시스템을 사용할 수 있습니다. TypeScript의 `console.error()`는 내부적으로 Node.js나 브라우저의 `stderr` 스트림을 통해 메시지를 출력합니다.

## See Also (더 보기)
- [Node.js Documentation on console.error()](https://nodejs.org/api/console.html#consoleerrordata-args)
- [MDN Web Docs on Console](https://developer.mozilla.org/en-US/docs/Web/API/Console/error)
- [Understanding Streams in Node.js](https://nodejs.org/api/stream.html)
