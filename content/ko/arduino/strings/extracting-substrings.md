---
date: 2024-01-20 17:45:29.708465-07:00
description: "How to: (\uBC29\uBC95) \uC544\uB450\uC774\uB178\uC5D0\uC11C \uBB38\uC790\
  \uC5F4 \uC870\uC791\uC740 \uC784\uBCA0\uB514\uB4DC \uC2DC\uC2A4\uD15C\uC5D0\uC11C\
  \ \uC911\uC694\uD55C \uB2A5\uB825 \uC911 \uD558\uB098\uC785\uB2C8\uB2E4. \uACFC\uAC70\
  \uC5D0\uB294 \uBA54\uBAA8\uB9AC \uC81C\uC57D \uB54C\uBB38\uC5D0 \uBCF5\uC7A1\uD55C\
  \ \uBB38\uC790\uC5F4 \uC791\uC5C5\uC744 \uD53C\uD588\uC9C0\uB9CC, \uD604\uB300\uC801\
  \ \uC544\uB450\uC774\uB178\uB294 \uB354 \uB9CE\uC740 \uBA54\uBAA8\uB9AC\uC640 \uCC98\
  \uB9AC \uB2A5\uB825\uC744 \uAC00\uC9C0\uACE0 \uC788\uC5B4 \uBB38\uC790\uC5F4 \uC791\
  \uC5C5\uC774 \uB354 \uC26C\uC6CC\uC84C\uC2B5\uB2C8\uB2E4. \uB300\uC548\uC73C\uB85C\
  \uB294 C \uC2A4\uD0C0\uC77C\uC758\u2026"
isCJKLanguage: true
lastmod: '2024-04-05T21:53:57.239162-06:00'
model: gpt-4-1106-preview
summary: "(\uBC29\uBC95) \uC544\uB450\uC774\uB178\uC5D0\uC11C \uBB38\uC790\uC5F4 \uC870\
  \uC791\uC740 \uC784\uBCA0\uB514\uB4DC \uC2DC\uC2A4\uD15C\uC5D0\uC11C \uC911\uC694\
  \uD55C \uB2A5\uB825 \uC911 \uD558\uB098\uC785\uB2C8\uB2E4."
title: "\uBD80\uBD84 \uBB38\uC790\uC5F4 \uCD94\uCD9C"
weight: 6
---

## How to: (방법)
```arduino
String fullText = "Hello from Arduino land!";
String subText = fullText.substring(6, 10);

void setup() {
  Serial.begin(9600);
  Serial.println(subText); // "from"
}

void loop() {
  // Nothing to loop over here.
}
```

## Deep Dive (심층 탐구)
아두이노에서 문자열 조작은 임베디드 시스템에서 중요한 능력 중 하나입니다. 과거에는 메모리 제약 때문에 복잡한 문자열 작업을 피했지만, 현대적 아두이노는 더 많은 메모리와 처리 능력을 가지고 있어 문자열 작업이 더 쉬워졌습니다. 대안으로는 C 스타일의 char 배열과 함수가 있습니다만, String 클래스는 사용하기 편하며, 높은 수준의 문자열 조작을 제공합니다. 부분 문자열 추출시 주의할 점은 메모리 관리입니다; String 객체는 동적으로 메모리 할당을 관리하기에 메모리 누수가 발생할 수 있습니다.

## See Also (참고 자료)
- `substring()` 메소드 사용 방법 상세: [https://www.arduino.cc/reference/en/language/variables/data-types/string/functions/substring/](https://www.arduino.cc/reference/en/language/variables/data-types/string/functions/substring/)
