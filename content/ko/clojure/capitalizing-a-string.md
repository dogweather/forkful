---
title:                "문자열 대문자화"
aliases:
- ko/clojure/capitalizing-a-string.md
date:                  2024-02-03T19:04:55.981216-07:00
model:                 gpt-4-0125-preview
simple_title:         "문자열 대문자화"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/clojure/capitalizing-a-string.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
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
