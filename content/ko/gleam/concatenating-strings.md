---
title:                "문자열 연결하기"
html_title:           "Gleam: 문자열 연결하기"
simple_title:         "문자열 연결하기"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/gleam/concatenating-strings.md"
---

{{< edit_this_page >}}

## 무엇인가요? 왜 해야 하나요?
문자열을 연결(concatenating)한다는 것은 간단하게 말하자면, 문자열을 결합하는 것입니다. 이런 작업을 프로그래머가 하는 이유는 자주 사용되는 구문을 쉽게 생성하고 코딩의 복잡성을 줄이기 위해서입니다.

## 하는 방법:
```Gleam
let welcome = "언니네 가게에 오신걸 환영합니다!"
let message = "수박이 맛좋아서"
IO.print(welcome ++ " " ++ message)
```
위 코드를 실행하면 ```언니네 가게에 오신걸 환영합니다! 수박이 맛좋아서```라는 출력을 볼 수 있습니다.

## 깊이 파고들기:
1. 문자열 연결(concatenation)은 컴퓨터 과학에서 매우 일반적이고 중요한 작업입니다. 이는 기존의 문자열을 변경하지 않기 때문에 프로그래밍에서 불변성(immunity)을 유지하는 데 도움이 됩니다.
2. Gleam에서는 ```++```연산자를 사용하여 문자열을 연결할 수 있습니다. 다른 언어에서는 ```+```기호를 사용하거나 ```concat()```같은 함수를 사용하기도 합니다.
3. 일부 언어에서는 문자열을 연결할 때 많은 자원이 소비된다는 문제가 있습니다. 하지만 Gleam은 문자열을 빠르게 연결하는 최적화 알고리즘을 사용하여 이 문제를 해결합니다.

## 더 알아보기:
- [Gleam 공식 문서](https://gleam.run/)에서 문자열 연결하기에 대한 더 자세한 정보를 확인할 수 있습니다.
- 다른 언어에서도 문자열 연결에 대한 다양한 방법을 배울 수 있습니다. 예를 들어 Python에서는 ```+```기호를 사용하며, Java에서는 ```concat()``` 함수를 사용합니다.