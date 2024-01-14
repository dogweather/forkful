---
title:    "Haskell: 문자열의 길이 찾기"
keywords: ["Haskell"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/ko/haskell/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## 왜

Haskell은 함수형 프로그래밍 언어로써 많은 사람들이 놀라운 새로운 개념과 기술을 배우고 싶어합니다. 그 중 하나가 문자열의 길이를 찾는 것입니다! 이 작업을 하는 방법을 배워 더 많은 도전적인 프로그래밍을 할 수 있도록 해주세요.

## 어떻게

이 작업을 수행하는 가장 간단한 방법은 `length` 함수를 사용하는 것입니다. ```Haskell
length "한글" -- 출력: 2
length "hello" -- 출력: 5
```

이 함수는 문자열의 길이를 반환하는 것으로 검색할 수 있는 다른 것들과 비슷합니다.

## 깊이 파고들기

Haskell은 기본적으로 이미 정의된 `length` 함수를 제공하며, 인자로 들어오는 값에 따라 길이를 어떻게 계산하는지 다릅니다. 일반적인 문자열의 경우, 실제로 문자의 개수를 세어 길이를 계산합니다. 하지만 리스트나 튜플의 경우, 각 요소의 개수를 세어 길이를 반환합니다. 이렇게 다양한 데이터 타입에 대해서 `length` 함수를 사용할 수 있다는 것이 Haskell의 장점 중 하나입니다.

## 관련 자료들

* [Haskell 공식 문서](https://www.haskell.org/documentation/)
* [Learn You a Haskell](http://learnyouahaskell.com/)
* [Haskell Tutorial](https://www.tutorialspoint.com/haskell/index.htm)