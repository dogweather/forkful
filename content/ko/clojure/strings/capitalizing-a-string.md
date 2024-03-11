---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:04:55.981216-07:00
description: "\uBB38\uC790\uC5F4\uC744 \uB300\uBB38\uC790\uB85C \uB9CC\uB4E0\uB2E4\
  \uB294 \uAC83\uC740 \uBB38\uC790\uC5F4\uC758 \uCCAB \uAE00\uC790\uB97C \uB300\uBB38\
  \uC790\uB85C \uBCC0\uACBD\uD558\uBA74\uC11C \uB098\uBA38\uC9C0 \uBB38\uC790\uC5F4\
  \uC740 \uBCC0\uACBD\uD558\uC9C0 \uC54A\uB294 \uAC83\uC744 \uC758\uBBF8\uD569\uB2C8\
  \uB2E4. \uD504\uB85C\uADF8\uB798\uBA38\uB4E4\uC740 \uD2B9\uD788 \uC774\uB984\uACFC\
  \ \uC7A5\uC18C\uB098 \uC0AC\uC6A9\uC790 \uC778\uD130\uD398\uC774\uC2A4\uC758 \uBB38\
  \uBC95 \uADDC\uCE59\uC744 \uC900\uC218\uD558\uAE30 \uC704\uD574 \uB370\uC774\uD130\
  \ \uC77C\uAD00\uC131\uC744 \uBCF4\uC7A5\uD558\uAE30 \uC704\uD574 \uBB38\uC790\uC5F4\
  \ \uB300\uBB38\uC790\uD654\uB97C \uC790\uC8FC \uC218\uD589\uD569\uB2C8\uB2E4."
lastmod: '2024-03-11T00:14:28.551456-06:00'
model: gpt-4-0125-preview
summary: "\uBB38\uC790\uC5F4\uC744 \uB300\uBB38\uC790\uB85C \uB9CC\uB4E0\uB2E4\uB294\
  \ \uAC83\uC740 \uBB38\uC790\uC5F4\uC758 \uCCAB \uAE00\uC790\uB97C \uB300\uBB38\uC790\
  \uB85C \uBCC0\uACBD\uD558\uBA74\uC11C \uB098\uBA38\uC9C0 \uBB38\uC790\uC5F4\uC740\
  \ \uBCC0\uACBD\uD558\uC9C0 \uC54A\uB294 \uAC83\uC744 \uC758\uBBF8\uD569\uB2C8\uB2E4\
  . \uD504\uB85C\uADF8\uB798\uBA38\uB4E4\uC740 \uD2B9\uD788 \uC774\uB984\uACFC \uC7A5\
  \uC18C\uB098 \uC0AC\uC6A9\uC790 \uC778\uD130\uD398\uC774\uC2A4\uC758 \uBB38\uBC95\
  \ \uADDC\uCE59\uC744 \uC900\uC218\uD558\uAE30 \uC704\uD574 \uB370\uC774\uD130 \uC77C\
  \uAD00\uC131\uC744 \uBCF4\uC7A5\uD558\uAE30 \uC704\uD574 \uBB38\uC790\uC5F4 \uB300\
  \uBB38\uC790\uD654\uB97C \uC790\uC8FC \uC218\uD589\uD569\uB2C8\uB2E4."
title: "\uBB38\uC790\uC5F4 \uB300\uBB38\uC790\uD654"
---

{{< edit_this_page >}}

## 무엇 & 왜?
문자열을 대문자로 만든다는 것은 문자열의 첫 글자를 대문자로 변경하면서 나머지 문자열은 변경하지 않는 것을 의미합니다. 프로그래머들은 특히 이름과 장소나 사용자 인터페이스의 문법 규칙을 준수하기 위해 데이터 일관성을 보장하기 위해 문자열 대문자화를 자주 수행합니다.

## 방법:
Clojure는 JVM 언어이기 때문에 Java String 메소드를 직접 사용할 수 있습니다. 다음은 Clojure에서 문자열을 대문자로 만드는 방법의 기본 예입니다:

```clojure
(defn capitalize-string [s]
  (if (empty? s)
    s
    (str (clojure.string/upper-case (subs s 0 1)) (subs s 1))))

(capitalize-string "hello world!") ; => "Hello world!"
```

Clojure는 문자열을 대문자로 만드는 데 특별히 만들어진 함수를 포함하고 있지 않지만, 보여진 것처럼 `clojure.string/upper-case`, `subs`, 그리고 `str` 함수들을 결합하여 쉽게 이를 달성할 수 있습니다.

더 간결한 해결책과 더 복잡한 문자열 조작을 처리하기 위해서는 제삼자 라이브러리로 전환할 수 있습니다. Clojure 생태계에서 인기 있는 라이브러리 중 하나는 `clojure.string`입니다. 하지만, 마지막 업데이트 시점에서, 이것은 핵심 Clojure 기능으로 보여진 것 외에 직접적인 `capitalize` 함수를 제공하지 않으므로, 위에서 보여진 방법이 대문자화를 위해 특별한 라이브러리를 추가로 불러오지 않고 사용할 수 있는 간단한 접근법입니다.

Clojure에서 Java 메소드와 상호 작용하는 문자열을 다룰 때는, Java 문자열과 실제로 작업하는 것이므로, 필요한 경우 Clojure 코드에서 직접 Java의 String 메소드 전체를 활용할 수 있다는 것을 기억하세요.
