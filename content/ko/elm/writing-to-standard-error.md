---
title:                "Elm: 표준 오류 출력에 쓰기"
simple_title:         "표준 오류 출력에 쓰기"
programming_language: "Elm"
category:             "Elm"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/elm/writing-to-standard-error.md"
---

{{< edit_this_page >}}

# 왜 standard error를 사용하는가?

일반적으로 프로그램을 작성하다 보면 디버깅이 필요할 때가 있습니다. 이때 특정 부분의 문제를 찾거나 실행 중에 발생한 오류를 수정하기 위해서는 오류가 발생한 곳을 찾아내야 합니다. Elm에서는 이를 돕기 위해 standard error를 사용합니다.

# 어떻게 사용할까?

## 예제 1: 정수 덧셈

```Elm
sum : Int -> Int -> Int
sum a b =
    a + b

main =
    Debug.log "결과값:" (sum 3 5)
```

위의 코드는 `sum` 함수를 정의하고, `main` 함수에서 결과값을 표시하는 예제입니다. `Debug.log` 함수를 사용하여 결과값을 출력하고, 실행 결과는 아래와 같습니다.

```
결과값: 8
```

여기서 `Debug.log` 함수는 `Int` 값을 받아서 standard error에 출력하는 역할을 합니다.

## 예제 2: 리스트의 인덱스 접근

```Elm
list : List String
list =
    [ "사과", "바나나", "딸기", "포도" ]

main =
    Debug.log "인덱스 2번째 값:" (List.get 2 list)
```

위의 코드는 `list`에 저장된 과일 이름 중 2번째 값인 "딸기"를 출력하는 예제입니다. `List.get` 함수를 사용하여 리스트의 특정 인덱스 값을 가져오고, 결과값은 아래와 같습니다.

```
인덱스 2번째 값: Just "딸기"
```

위에서와 마찬가지로 `Debug.log` 함수가 standard error에 결과값을 출력합니다.

# 자세히 살펴보기

`Debug.log` 함수는 말 그대로 결과값을 로그에 출력하는 역할을 합니다. Elm 코드를 실행할 때, `fun main ->` 뒤에 있는 코드 부분이 실행되고, 이때 `Debug.log` 함수가 사용되었다면 결과값이 standard error에 출력됩니다. 이를 이용하여 코드의 실행 결과를 확인하거나, 특정 값을 추적할 수 있습니다. 또한 `Debug.log` 함수를 사용하지 않는 경우에는 결과값이 standard error에 출력되지 않으므로, 코드를 실행할 때 꼭 확인해야 합니다.

# 관련 자료

- See Also:
    - [Elm 공식 가이드](https://guide.elm-lang.org/)
    - [Elm 커뮤니티 포럼](https://discourse.elm-lang.org/)
    - [Elm 책 추천 5선](https://openbookshelf.org/book/5election-5-elm-books)