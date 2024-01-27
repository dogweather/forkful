---
title:                "문자열 대문자로 변환하기"
date:                  2024-01-19
html_title:           "Arduino: 문자열 대문자로 변환하기"
simple_title:         "문자열 대문자로 변환하기"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Strings"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/clojure/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## What & Why? (무엇이며 왜?)
문자열을 대문자화하는 것은 모든 문자를 대문자로 바꾸는 걸 말해요. 데이터 형식을 통일하거나, 사용자 입력을 정리할 때 종종 쓰죠.

## How to: (어떻게 하나요?)
Clojure에서 대문자화는 쉬워요. `clojure.string` 라이브러리의 `upper-case` 함수 사용해 보겠습니다.

```clojure
(require '[clojure.string :as str])

;; 문자열을 대문자로 변환합니다.
(str/upper-case "hello clojure")
;; => "HELLO CLOJURE"

;; 다른 예:
(str/upper-case "안녕하세요")
;; => "안녕하세요"
```

참고로 한글은 대소문자 구분이 없어 대문자로 변경되지 않아요.

## Deep Dive (깊이 알아보기)
대문자화는 프로그래밍 역사에서 오래된 개념입니다. UNIX 시스템에서 대소문자 구분 없이 사용자 이름을 처리할 때부터 쓰였죠. 또한, 알림 메시지나 조건필터에도 적용됩니다.

대안으로는 `capitalize` 함수가 있는데, 이는 첫 글자만 대문자로 바꿉니다. 예를 들면, `Clojure`라는 단어를 `"clojure"`에서 `"Clojure"`로 바꾸죠.

구현 세부사항에서는 각 문자의 ASCII 또는 Unicode 값을 체크하여 대문자에 해당하는 값을 찾는 과정을 거칩니다. Clojure는 자바 플랫폼 위에서 돌아가기 때문에, `String` 클래스에 있는 대문자화 기능을 내부적으로 활용합니다.

## See Also (더 알아보기)
- [Clojure Documentation](https://clojure.org/guides/getting_started)
- [`clojure.string` API](https://clojuredocs.org/clojure.string/upper-case)
- [Java String Documentation](https://docs.oracle.com/javase/7/docs/api/java/lang/String.html)
