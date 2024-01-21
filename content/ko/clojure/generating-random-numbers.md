---
title:                "난수 생성하기"
date:                  2024-01-20T17:48:45.167127-07:00
model:                 gpt-4-1106-preview
simple_title:         "난수 생성하기"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/clojure/generating-random-numbers.md"
---

{{< edit_this_page >}}

## What & Why? (무엇인가요? 왜 사용하나요?)
랜덤 숫자 생성은 예상할 수 없는 숫자를 만드는 과정입니다. 프로그래머들은 게임, 시뮬레이션, 보안 등에서 예측 불가능성이 요구될 때 이를 사용합니다.

## How to: (어떻게 하나요?)
Clojure에서 랜덤 숫자를 생성하는 것은 쉽습니다. `rand`, `rand-int`, `rand-nth` 함수를 이용해 보세요.

```Clojure
;; 0과 1 사이의 랜덤 실수를 생성합니다.
(rand)

;; 0부터 9까지의 랜덤 정수를 생성합니다.
(rand-int 10)

;; 리스트에서 랜덤하게 하나의 요소를 선택합니다.
(rand-nth [100 200 300 400 500])
```

실행 예시:

```Clojure
(rand)        ; => 0.7095281303457979
(rand-int 10) ; => 7
(rand-nth [100 200 300 400 500]) ; => 300
```

## Deep Dive (심층 분석)
Clojure의 랜덤 함수들은 자바의 `java.util.Random` 클래스를 기반으로 합니다. 역사적으로 랜덤 수 생성기는 통계학, 암호학, 컴퓨터 시뮬레이션 등 여러 분야에서 중요한 역할을 해왔습니다. 정말로 '무작위' 생성이 필요하다면, `java.security.SecureRandom`과 같이 더 강력한 난수 생성기를 사용할 수도 있습니다. 이러한 도구들은 규칙적인 패턴을 덜 생성하고 예측하기 어렵습니다.

## See Also (참고 자료)
- Clojure 공식 문서의 랜덤 함수 설명: [Clojure - Random](https://clojure.github.io/clojure/clojure.core-api.html#clojure.core/rand)
- 자바 `Random` 클래스: [java.util.Random](https://docs.oracle.com/javase/8/docs/api/java/util/Random.html)
- 보안 강화를 위한 `SecureRandom` 클래스: [java.security.SecureRandom](https://docs.oracle.com/javase/8/docs/api/java/security/SecureRandom.html)