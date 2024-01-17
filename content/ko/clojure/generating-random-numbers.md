---
title:                "랜덤 숫자 생성하기"
html_title:           "Clojure: 랜덤 숫자 생성하기"
simple_title:         "랜덤 숫자 생성하기"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/clojure/generating-random-numbers.md"
---

{{< edit_this_page >}}

지금 코딩의 세계에서 무작위성은 매우 중요합니다. 많은 경우에 프로그래머들은 코드를 작성하는 과정에서 임의의 값을 생성하고 싶어합니다. 이를 해결하기 위해 Clojure는 무작위 숫자를 생성하는 기능을 제공합니다. 이 기능을 사용하면 개발자들은 코드를 더욱 다양하게 작성하고, 더 많은 환경에서 테스트할 수 있습니다.

## 무엇이며 그 이유?
무작위 숫자를 생성하는 것은 코드에서 예상치 못한 동작을 시뮬레이션하거나 여러 가지 테스트를 수행하는데 아주 효율적인 방법입니다. 예를 들어, 어떤 게임의 결과를 시뮬레이션하거나 엄청난 양의 데이터를 생성할 때 유용합니다.

## 사용 방법:
Clojure에서는 ```(rand)``` 함수를 사용하여 무작위 실수를 생성할 수 있습니다. 만약 0과 1사이의 숫자만을 원한다면, ```(rand)``` 대신에 ```(rand 1.0)``` 을 사용하면 됩니다. 또는 특정 범위 내에서 무작위 정수를 생성하려면, ```(rand-int n)``` 함수를 사용하면 됩니다.

예시:
```Clojure
(rand) ;; 0.781654092
(rand 10) ;; 5 (0과 10 사이의 정수)
(rand-int 100) ;; 64 (0과 100 사이의 정수)
```

## 깊이 파헤치기:
Clojure는 Java의 ```java.util.Random``` 클래스를 사용하여 무작위 숫자를 생성합니다. 이 클래스는 시드 값을 기준으로 무작위 값을 생성하므로, 시드 값을 설정하지 않으면 실행마다 같은 무작위 값을 반환합니다. 따라서 시드 값이 코드의 여러 부분에서 동일하게 유지되어야 한다는 점에 유의해야 합니다.

Clojure에서는 또한 무작위 검사를 지원하기 위해 ```rand-nth```와 ```rand-uniform``` 함수를 제공합니다. 더 많은 정보를 원한다면 [공식 문서](https://clojure.github.io/clojure/clojure.core-api.html#clojure.core/rand)를 참조해주세요.

## 참고 자료:
Clojure 공식 문서: <https://clojure.github.io/clojure/clojure.core-api.html#clojure.core/rand>