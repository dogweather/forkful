---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:04:55.981216-07:00
description: "\uBC29\uBC95: Clojure\uB294 JVM \uC5B8\uC5B4\uC774\uAE30 \uB54C\uBB38\
  \uC5D0 Java String \uBA54\uC18C\uB4DC\uB97C \uC9C1\uC811 \uC0AC\uC6A9\uD560 \uC218\
  \ \uC788\uC2B5\uB2C8\uB2E4. \uB2E4\uC74C\uC740 Clojure\uC5D0\uC11C \uBB38\uC790\uC5F4\
  \uC744 \uB300\uBB38\uC790\uB85C \uB9CC\uB4DC\uB294 \uBC29\uBC95\uC758 \uAE30\uBCF8\
  \ \uC608\uC785\uB2C8\uB2E4."
lastmod: '2024-03-13T22:44:54.636539-06:00'
model: gpt-4-0125-preview
summary: "Clojure\uB294 JVM \uC5B8\uC5B4\uC774\uAE30 \uB54C\uBB38\uC5D0 Java String\
  \ \uBA54\uC18C\uB4DC\uB97C \uC9C1\uC811 \uC0AC\uC6A9\uD560 \uC218 \uC788\uC2B5\uB2C8\
  \uB2E4."
title: "\uBB38\uC790\uC5F4 \uB300\uBB38\uC790\uD654"
weight: 2
---

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
