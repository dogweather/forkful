---
title:                "테스트 작성하기"
aliases:
- ko/javascript/writing-tests.md
date:                  2024-02-03T19:31:16.075158-07:00
model:                 gpt-4-0125-preview
simple_title:         "테스트 작성하기"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/javascript/writing-tests.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## 무엇 & 왜?

자바스크립트에서 테스트 작성은 코드가 예상대로 동작하는지 확인하기 위해 자동 스크립트를 만드는 작업을 말합니다. 이는 애플리케이션의 신뢰성과 유지보수성을 크게 향상시킬 수 있습니다. 프로그래머들은 이를 통해 버그를 조기에 발견하고, 코드 리팩토링을 용이하게 하며, 새로운 기능이 기존 기능을 해치지 않도록 합니다.

## 방법:

### 네이티브 접근법 (Jest 사용)

Jest는 자바스크립트에서 단위 테스트를 작성하기 위한 친근한 API를 제공하는 인기 있는 테스트 프레임워크입니다. 최소한의 설정이 필요하며, 모의 함수, 타이머, 스냅샷 테스팅과 같은 기능을 제공합니다.

1. **설치**:

```bash
npm install --save-dev jest
```

2. **간단한 테스트 작성**:

`sum.test.js`라는 파일을 생성합니다:

```javascript
const sum = require('./sum'); // 이 함수는 단순히 두 숫자를 더한다고 가정

test('1 + 2를 더하면 3이 된다', () => {
  expect(sum(1, 2)).toBe(3);
});
```

3. **테스트 실행**:

```bash
npx jest
```

**샘플 출력:**

```plaintext
PASS  ./sum.test.js
✓ 1 + 2를 더하면 3이 된다 (5ms)
```

### 비동기 코드 테스트

Jest는 프로미스와 async/await 구문을 테스트하기 쉽게 만듭니다:

```javascript
// asyncSum.js
async function asyncSum(a, b) {
  return Promise.resolve(a + b);
}

// asyncSum.test.js
test('비동기 덧셈이 작동한다', async () => {
  await expect(asyncSum(1, 2)).resolves.toBe(3);
});

```

### 타사 라이브러리 사용하기 (Mocha & Chai)

Mocha는 또 다른 인기 있는 테스트 프레임워크로, 보다 표현력 있는 테스트를 위해 주장 라이브러리인 Chai와 함께 종종 사용됩니다.

1. **설치**:

```bash
npm install --save-dev mocha chai
```

2. **Mocha와 Chai로 테스트 작성**:

`calculate.test.js`를 생성합니다:

```javascript
const chai = require('chai');
const expect = chai.expect;

const calculate = require('./calculate'); // 간단한 계산 모듈

describe('Calculate', function() {
  it('두 값을 더해야 한다', function() {
    expect(calculate.sum(5, 2)).to.equal(7);
  });
});
```

3. **Mocha로 테스트 실행**:

`package.json`에 스크립트를 추가합니다:

```json
"scripts": {
  "test": "mocha"
}
```

그리고 실행합니다:

```bash
npm test
```

**샘플 출력:**

```plaintext
  Calculate
    ✓ 두 값을 더해야 한다


  1 passing (8ms)
```

이 예시들은 자바스크립트에서 기본적인 테스트 작성 및 실행을 보여줍니다. Jest나 Mocha와 Chai 같은 테스트 프레임워크를 채택하면 애플리케이션 테스트를 위한 견고한 기반을 제공하여, 업데이트와 리팩토링을 거치며 코드가 의도대로 기능하도록 도움을 줄 수 있습니다.
