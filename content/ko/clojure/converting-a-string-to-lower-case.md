---
title:                "문자열을 소문자로 변환하기"
html_title:           "Bash: 문자열을 소문자로 변환하기"
simple_title:         "문자열을 소문자로 변환하기"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/clojure/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## 무엇 & 왜?

문자열을 소문자로 변환한다는 것은 모든 대문자가 소문자로 변경되는 작업입니다. 프로그래머들이 이를 수행하는 주요 이유는 데이터 정규화와 비교에 있습니다. 

## 어떻게: 

```Clojure
(defn lowercase-string [s]
  (.toLowerCase s))

(lowercase-string "HELLO, WORLD!") ;; => "hello, world!"
```

위 코드는 Clojure에서 문자열을 소문자로 변경하는 방법입니다. 'toLowerCase' 메서드는 문자열을 소문자로 변경합니다.

## 깊게 알아보기

이 연산은 ASCII와 같은 문자 세트에서 처음 시작되었습니다. 이 방법은 간단하다는 장점이 있지만, 모든 언어와 문자셋에 적용 가능한 것은 아닙니다. 때때로 특정 언어에서는 특수 규칙이 적용되기도 합니다. 

대안적으로, 로케일에 따라 라이브러리를 사용하여 문자열을 소문자로 변환하거나 대소문자를 무시하는 비교를 수행할 수 있습니다. Clojure에서는 다른 함수나 라이브러리를 사용하여 문자열을 소문자로 변환하는 한 가지 대안은 ICU4J 라이브러리를 사용하는 것입니다. 

소문자 변환 작업은 실제로 JVM의 `Character.toLowerCase` 메소드를 사용하여 수행됩니다. 이 메서드는 유니코드 문자를 소문자로 변환합니다.

## 참고 자료

1. Clojure 소스 코드: [https://clojure.org/]
2. `Character.toLowerCase` 메서드 관련 문서: [https://docs.oracle.com/javase/8/docs/api/java/lang/Character.html#toLowerCase-char-]
3. ICU4J 라이브러리 문서: [http://userguide.icu-project.org/strings]