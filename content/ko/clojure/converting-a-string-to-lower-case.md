---
title:                "문자열을 소문자로 변환하기"
date:                  2024-01-20T17:38:36.337672-07:00
model:                 gpt-4-1106-preview
simple_title:         "문자열을 소문자로 변환하기"

category:             "Clojure"
tag:                  "Strings"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/clojure/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## What & Why? (무엇과 왜?)
문자열을 소문자로 변환한다는 건, 모든 대문자를 해당하는 소문자로 바꾸는 것입니다. 프로그래머들은 데이터를 표준화하거나 대소문자에 민감하지 않은 비교를 수행하기 위해서 이 작업을 합니다.

## How to (방법):
Clojure에서 문자열을 소문자로 변환하기 위해 `clojure.string/lower-case` 함수를 사용할 수 있습니다.

```Clojure
(require '[clojure.string :as str])

;; 문자열을 소문자로 변환
(str/lower-case "Hello, World!")
```

출력 결과:
```
"hello, world!"
```

## Deep Dive (심층 분석):

1. **역사적 맥락**: 문자열 처리는 컴퓨터 프로그래밍의 오래된 도전 주제 중 하나입니다. 초기부터 대소문자 변환은 텍스트 검색, 정렬 및 매핑에 필수적인 기능이었습니다.

2. **대안들**: `clojure.string/lower-case` 외에도, 자바의 `String` 메소드를 직접 사용할 수도 있습니다 (`(.toLowerCase "HELLO")`). 커스텀 함수를 작성해 변환을 수행할 수도 있지만 효율성과 라이브러리의 검증된 기능을 고려할 때 필수적이지는 않습니다.

3. **구현 세부 사항**: `clojure.string/lower-case`는 내부적으로 자바 `String` 클래스의 `toLowerCase` 메소드를 호출합니다. 따라서 이 메소드는 자바의 로케일 기반 변환 규칙을 그대로 사용합니다. 특별한 언어 정책이 필요한 경우 로케일을 명시적으로 설정할 수 있습니다.

## See Also (추가 자료):

- 공식 Clojure 문자열 API 문서: [https://clojure.github.io/clojure/clojure.string-api.html](https://clojure.github.io/clojure/clojure.string-api.html)
- 자바 `String` 메소드 `toLowerCase`: [https://docs.oracle.com/en/java/javase/11/docs/api/java.base/java/lang/String.html#toLowerCase()](https://docs.oracle.com/en/java/javase/11/docs/api/java.base/java/lang/String.html#toLowerCase())
- Clojure 문자열 처리 기초: [https://clojure.org/guides/weird_characters](https://clojure.org/guides/weird_characters)
