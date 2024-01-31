---
title:                "테스트 작성하기"
date:                  2024-01-19
simple_title:         "테스트 작성하기"

tag:                  "Testing and Debugging"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/javascript/writing-tests.md"
---

{{< edit_this_page >}}

## What & Why? (무엇인가요? 왜 중요한가요?)
테스트 작성은 코드가 예상대로 작동하는지 확인하는 과정입니다. 프로그래머들은 버그를 찾아내고, 소프트웨어 품질을 보장하며, 새로운 기능 수정 시 안정성을 유지하기 위해 테스트를 작성합니다.

## How to: (어떻게 하나요?)
```Javascript
// 가장 기본적인 테스트 예시 using Jest
const sum = require('./sum'); // sum.js에서 함수를 임포트합니다.

test('2와 3을 더하면 5가 나와야 합니다', () => {
  expect(sum(2, 3)).toBe(5);
});
```
```Javascript
// sum.js
function sum(a, b) {
  return a + b;
}

module.exports = sum;
```
실행 결과:
```
PASS  ./sum.test.js
✓ 2와 3을 더하면 5가 나와야 합니다 (1ms)
```

## Deep Dive (심도 있는 분석)
역사적으로 자바스크립트 테스팅은 JUnit 같은 자바 테스팅 프레임워크를 모델로 하여 발전해왔습니다. Jest, Mocha, Jasmine 등과 같은 테스팅 프레임워크들이 널리 사용됩니다. 이 툴들은 다양한 테스팅 유형(단위 테스트, 통합 테스트, 종단 간 테스트)을 지원합니다. 테스트 작성은 테스트 주도 개발(TDD)이나 행동 주도 개발(BDD) 같은 개발 방법론을 통해 코드 디자인과 구현 과정에 영향을 미칩니다.

## See Also (관련 자료)
- [Jest 공식 문서](https://jestjs.io)
- [Mocha 공식 웹사이트](https://mochajs.org)
- [Jasmine 공식 웹사이트](https://jasmine.github.io/)
- [테스트 주도 개발(TDD)에 대한 소개](https://en.wikipedia.org/wiki/Test-driven_development)
