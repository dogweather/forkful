---
title:                "Clojure: 문자열의 길이 찾기"
simple_title:         "문자열의 길이 찾기"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/clojure/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

# 왜?

문자열의 길이를 찾는 것이 왜 중요한지 궁금하신가요? 문자열의 길이는 많은 프로그래밍 작업에서 중요한 역할을 합니다. 예를 들어, 입력받은 사용자의 이름이 유효한지 확인하고, 문자열의 앞뒤에 있는 공백을 제거하고 싶을 때 등등 많은 경우에 문자열의 길이를 알아야 할 수 있기 때문입니다.

# 방법

문자열의 길이를 찾는 가장 간단한 방법은 `count` 함수를 사용하는 것입니다. `count` 함수는 주어진 문자열의 길이를 반환해줍니다. 예를 들어, 만약 "Hello"라는 문자열의 길이를 알고 싶다면, 다음과 같이 코드를 작성할 수 있습니다.

```Clojure
(count "Hello")
```

위 코드의 출력 값은 5가 될 것입니다. 또 다른 예를 들어보면, "안녕하세요"라는 문자열의 길이를 알고 싶다면 다음과 같이 작성할 수 있습니다.

```Clojure
(count "안녕하세요")
```

위 코드의 출력 값은 5가 될 것입니다. 이는 Clojure가 모든 유니코드 문자를 하나의 문자로 취급하기 때문에 "안녕하세요" 라는 문자열의 길이가 5로 나오는 것입니다.

# 깊게 파고들기

물론 `count` 함수를 사용하는 것이 가장 간단하지만, 문자열의 길이를 찾는 데에는 다른 방법도 있습니다. 예를 들어, `seq` 함수를 사용해서 문자열을 일렬로 나열한 뒤, 해당 일렬로 나열된 문자열의 길이를 계산하는 방법이 있습니다. 이렇게 하면 유니코드 문자를 분리해서 세는 것이 가능해집니다.

```Clojure
(count (seq "안녕하세요"))
```

위 코드의 출력 값은 3이 될 것입니다. 하지만, 이 방법은 메모리를 많이 사용하므로 긴 문자열을 다루는 경우에는 추천되지 않습니다.

# 관련 자료

* [ClojureDocs: count 함수](https://clojuredocs.org/clojure.core/count)
* [ClojureDocs: seq 함수](https://clojuredocs.org/clojure.core/seq)
* [Online Clojure REPL](https://www.replit.com/languages/clojure)