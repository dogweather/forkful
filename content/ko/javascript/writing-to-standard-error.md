---
title:                "표준 오류에 쓰는 작성"
html_title:           "Javascript: 표준 오류에 쓰는 작성"
simple_title:         "표준 오류에 쓰는 작성"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/javascript/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## 무엇인가요?

표준 에러를 기록하는 것은 프로그래머가 에러 메시지를 확인하고 코드에서 오류를 디버그하는 데 도움이 됩니다. 이것은 개발 과정에서 중요한 단계이며, 예상치 못한 문제를 해결하는 데 도움이 됩니다.

## 어떻게 하나요?

이 간단한 예제를 통해 Javascript에서 표준 에러를 기록하는 방법을 살펴보겠습니다.

```Javascript
console.error("이 메시지는 표준 에러로 기록됩니다.");
```

이 코드를 실행하면, 디버그 콘솔에서 에러 메시지가 나타날 것입니다. 이것은 보편적인 에러 처리 방법이며, 프로그래머들은 이를 사용하여 코드에서 발생하는 문제를 식별하고 해결할 수 있습니다.

## 깊이 들어가보기

표준 에러를 기록하는 것은 1977년 Unix 시스템에서 처음 사용되었습니다. 이전에는 파일로 출력되지 않고 디스크에 기록되었습니다. 현재는 모든 운영 체제에서 유사한 방식으로 작동하며, Javascript 또한 이 방식을 따릅니다.

대체로는 표준 출력을 사용하여 프로그램의 결과를 모니터링하는 것이 더 일반적인 방법입니다. 그러나 예외 상황에서는 표준 에러를 사용하여 별도의 로깅을 할 수 있습니다.

## 관련 자료 보기

- [자바스크립트 디버깅 가이드 (Google Developers)](https://developers.google.com/web/tools/chrome-devtools/javascript/)
- [Unix 입출력 기본 지식](https://www.cis.upenn.edu/~milom/cse240-Fall03/unixIO/unixIO-handout.html)