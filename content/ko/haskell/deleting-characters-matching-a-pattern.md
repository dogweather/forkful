---
title:    "Haskell: 패턴에 일치하는 문자 삭제하기"
keywords: ["Haskell"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/ko/haskell/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## 왜

컴퓨터 프로그래밍에서 우리는 종종 특정한 패턴을 가진 문자들을 삭제해야하는 경우가 있습니다. 이러한 작업은 데이터 정제, 필터링 또는 데이터 분석에 유용합니다. 하스켈로 이러한 작업을 어떻게 수행할 수 있는지 살펴보겠습니다.

## 어떻게

하스켈에서 문자 패턴을 삭제하는 방법은 다양합니다. 가장 일반적인 방법은 `filter` 함수를 사용하는 것입니다. 아래의 예제를 살펴보면 주어진 패턴에 맞지 않는 문자들을 삭제하는 방법을 보실 수 있습니다.

```Haskell
-- 입력: "abcdefg"
-- 출력: "bdf"

deletePattern :: String -> String
deletePattern str = filter (\c -> c `notElem` ['a','c','e']) str
```

위의 함수를 살펴보면, `filter`는 주어진 리스트에서 조건을 만족하는 요소들만 남기고 나머지는 제거하는 작업을 합니다. 여기서는 `notElem` 함수를 사용하여 리스트에 존재하지 않는 문자들만 남기고 나머지는 제거합니다.

## 딥 다이브

문자 패턴을 삭제하는 방법은 `filter` 함수 외에도 다양한 방법이 있습니다. 특정 패턴을 가진 문자열을 정규 표현식을 사용하여 삭제할 수도 있습니다. 또는 `string-utils` 라이브러리를 사용하여 패턴을 삭제하는 함수를 직접 정의할 수도 있습니다.

또한 주의할 점은 문자 패턴을 삭제할 때 지정된 순서가 중요하다는 것입니다. 예를 들어, 위의 예제에서 "car"라는 패턴을 삭제하고 싶은 경우 `notElem` 함수를 사용하면 "cdr"과 "carpet"과 같이 출처가 "car"인 문자들도 함께 삭제될 수 있습니다. 따라서 패턴을 삭제할 때는 지정된 순서에 유의해야합니다.

## 참고

- [Haskell.org](https://www.haskell.org/) - 하스켈 공식 웹사이트
- [Learn You a Haskell for Great Good](http://learnyouahaskell.com/) - 하스켈 입문자를 위한 온라인 책
- [Haskell Wiki](https://wiki.haskell.org/) - 하스켈 관련 정보를 공유하는 위키 페이지