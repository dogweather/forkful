---
date: 2024-01-20 17:45:29.708465-07:00
description: "\uBB38\uC790\uC5F4\uC5D0\uC11C \uBD80\uBD84 \uBB38\uC790\uC5F4\uC744\
  \ \uCD94\uCD9C\uD558\uB294 \uAC83\uC740 \uD2B9\uC815 \uB370\uC774\uD130 \uC870\uAC01\
  \uC744 \uC5BB\uAE30 \uC704\uD574 \uBB38\uC790\uC5F4\uC744 \uC790\uB974\uB294 \uACFC\
  \uC815\uC785\uB2C8\uB2E4. \uD504\uB85C\uADF8\uB798\uBA38\uB4E4\uC740 \uC815\uBCF4\
  \uB97C \uBD84\uC11D\uD558\uACE0, \uAC00\uACF5\uD558\uBA70, \uD2B9\uC815 \uC870\uAC74\
  \uC5D0 \uB9DE\uB294 \uB0B4\uC6A9\uC744 \uCC3E\uAE30 \uC704\uD574 \uC774 \uC791\uC5C5\
  \uC744 \uC218\uD589\uD569\uB2C8\uB2E4."
isCJKLanguage: true
lastmod: 2024-02-19 22:05:14.506096
model: gpt-4-1106-preview
summary: "\uBB38\uC790\uC5F4\uC5D0\uC11C \uBD80\uBD84 \uBB38\uC790\uC5F4\uC744 \uCD94\
  \uCD9C\uD558\uB294 \uAC83\uC740 \uD2B9\uC815 \uB370\uC774\uD130 \uC870\uAC01\uC744\
  \ \uC5BB\uAE30 \uC704\uD574 \uBB38\uC790\uC5F4\uC744 \uC790\uB974\uB294 \uACFC\uC815\
  \uC785\uB2C8\uB2E4. \uD504\uB85C\uADF8\uB798\uBA38\uB4E4\uC740 \uC815\uBCF4\uB97C\
  \ \uBD84\uC11D\uD558\uACE0, \uAC00\uACF5\uD558\uBA70, \uD2B9\uC815 \uC870\uAC74\uC5D0\
  \ \uB9DE\uB294 \uB0B4\uC6A9\uC744 \uCC3E\uAE30 \uC704\uD574 \uC774 \uC791\uC5C5\uC744\
  \ \uC218\uD589\uD569\uB2C8\uB2E4."
title: "\uBD80\uBD84 \uBB38\uC790\uC5F4 \uCD94\uCD9C"
---

{{< edit_this_page >}}

## What & Why? (무엇과 왜?)
문자열에서 부분 문자열을 추출하는 것은 특정 데이터 조각을 얻기 위해 문자열을 자르는 과정입니다. 프로그래머들은 정보를 분석하고, 가공하며, 특정 조건에 맞는 내용을 찾기 위해 이 작업을 수행합니다.

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
