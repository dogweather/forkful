---
title:                "표준 오류로 쓰기"
date:                  2024-01-19
html_title:           "Bash: 표준 오류로 쓰기"
simple_title:         "표준 오류로 쓰기"

tag:                  "Files and I/O"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/javascript/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## What & Why? (무엇이며, 왜?)
표준 에러(standard error)에 쓰기는 프로세스의 오류 메시지를 출력하는 방법입니다. 개발자들은 디버깅을 용이하게 하고 오류 메시지를 표준 출력(로그나 주된 결과물)과 분리하기 위해 이를 사용합니다.

## How to: (방법)
Javascript에서 표준 에러에 쓰려면 `console.error()` 또는 `process.stderr.write()`를 사용합니다.

```javascript
// console.error 사용 예시
console.error('에러 발생: 파일을 찾을 수 없습니다.');

// process.stderr.write 사용 예시
process.stderr.write('에러 발생: 데이터베이스 연결 실패.\n');
```

**출력 결과:**
```
에러 발생: 파일을 찾을 수 없습니다.
에러 발생: 데이터베이스 연결 실패.
```

## Deep Dive (심화 학습)
초기 컴퓨터 시스템에서는 표준 출력(std out)과 표준 에러(std err) 스트림을 구분하여 오류 메시지를 분리하고 특별하게 처리할 수 있도록 했습니다. 대안으로는 로깅 라이브러리나 파일로의 직접 기록이 있지만, 표준 에러는 실시간으로 오류를 관찰할 때 유용합니다. `console.error`는 내부적으로 `process.stderr.write`를 사용하여 확장된 기능(스택 트레이스 같은)을 제공하고, 포맷팅 옵션도 가집니다.

## See Also (참고 자료)
- Node.js 공식 문서의 콘솔(Console): https://nodejs.org/api/console.html
- Node.js 공식 문서의 프로세스(Process): https://nodejs.org/api/process.html#process_process_stderr
- 기초적인 Node.js 로깅 가이드: https://www.twilio.com/blog/guide-node-js-logging
