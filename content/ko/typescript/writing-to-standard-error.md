---
title:                "TypeScript: 표준 에러에 기록하는 방법"
simple_title:         "표준 에러에 기록하는 방법"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/typescript/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## 왜

표준 에러를 기록하는 것에 대해 왜 신경 써야할까요? 표준 에러는 프로그래밍 중에 발생하는 오류 메시지나 디버깅 정보를 담고 있습니다. 이 정보를 기록하지 않으면 프로그램의 동작을 올바르게 분석할 수 없고, 오류를 찾기 위해 더 많은 시간과 노력이 필요합니다.

## 방법

```TypeScript
process.stderr.write("에러 메시지");
```

위의 코드를 사용하면 표준 에러에 원하는 메시지를 기록할 수 있습니다. 내장 모듈인 `process`를 사용해야 하며, `write` 메소드를 이용해 메시지를 전달합니다. 이 방법은 터미널인 경우에는 에러 메시지를 터미널에 출력하고, 파일인 경우에는 해당 파일에 메시지를 기록합니다.

```TypeScript
console.error("에러 메시지");
```

또 다른 방법은 `console` 객체의 `error` 메소드를 이용하는 것입니다. 이 방법은 보통 개발 중에 프로그램의 상태를 확인하기 위해 사용하는 `console.log`와 유사하지만, `console.error`는 매개변수로 전달한 문자열을 표준 에러에 기록합니다.

## 더 깊게 들어가기

표준 에러의 사용은 단순히 오류 메시지를 기록하는 것 이상의 의미가 있습니다. 프로그램의 실행 중에 발생하는 오류나 예외는 항상 예측이 불가능합니다. 때문에 이러한 상황이 발생할 수 있다는 것을 염두에 두어야 합니다. 프로그램이 예측할 수 없는 상황에서도 예외를 기록한다면, 디버깅이 더 쉬워지고 프로그램의 안정성을 높일 수 있습니다.

그리고 표준 에러에 메시지를 기록할 때는 최대한 이해하기 쉬운 내용으로 작성하는 것이 좋습니다. 어떤 오류가 발생했는지 명확하게 설명하고, 해당 오류를 해결하기 위해 사용자에게 취할 수 있는 조치를 알려주는 것이 좋습니다.

## 본문에서 보기

이 포스트에서는 표준 에러를 기록하는 이유와 방법에 대해 알아보았습니다. 표준 에러를 기록하는 것은 프로그램의 안정성을 높이고 디버깅을 쉽게 만드는 중요한 요소입니다. 최대한 이해하기 쉽게 오류 메시지를 작성하는 것이 좋으며, 단순히 프로그램 내부에서는 아니라 사용자에게도 쉽게 이해할 수 있는 메시지를 전달하여 더 나은 사용자 경험을 제공하는 것이 중요합니다.

## 관련 포스트

- [Node.js 공식 문서: process.stderr](https://nodejs.org/dist/latest-v14.x/docs/api/process.html#process_process_stderr)
- [MDN 웹 문서: console.error()](https://developer.mozilla.org/ko/docs/Web/API/Console/error)
- [백엔드 개발 강좌 – 7일차: 디버깅 기술](https://wikibook.co.kr/backend-tutorials/7/)