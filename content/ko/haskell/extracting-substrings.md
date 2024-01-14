---
title:                "Haskell: 부분 문자열 추출하기"
programming_language: "Haskell"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/haskell/extracting-substrings.md"
---

{{< edit_this_page >}}

## 왜

문자열에서 부분 문자열을 추출하는 방법은 많은 이유로 인해 도움이 될 수 있습니다. 예를 들어, 텍스트 처리나 데이터 마이닝을 할 때 유용합니다. 또한 문자열을 다루는 알고리즘에도 적용할 수 있습니다.

## 추출하는 방법

Haskell에서 문자열에서 부분 문자열을 추출하는 방법은 간단합니다. `take` 함수를 사용하면 됩니다. 예를 들어, `"Hello, world!"` 문자열에서 `"Hello"` 부분 문자열을 추출하고 싶다면 다음과 같이 작성할 수 있습니다.

```Haskell
take 5 "Hello, world!"
-- 출력: "Hello"
```
위의 예시에서 `take` 함수의 첫 번째 인자는 추출하고자 하는 문자열의 길이를 나타내며, 두 번째 인자는 추출하고자 하는 문자열입니다.

또한 마지막 인덱스를 지정하여 원하는 위치부터 문자열을 추출할 수도 있습니다. 예를 들어, `"Hello, world!"` 문자열에서 `, world!` 부분 문자열을 추출하고 싶다면 다음과 같이 작성할 수 있습니다.

```Haskell
drop 6 "Hello, world!"
-- 출력: ", world!"
```

## 깊게 파고들기

`take` 함수를 사용하면 부분 문자열을 쉽게 추출할 수 있지만, 더 복잡한 작업을 하려면 좀 더 깊게 파고들어야 합니다. 예를 들어, 다른 문자열 처리 함수들과 조합하여 원하는 부분 문자열을 추출할 수도 있습니다. 또한, 문자열의 길이나 인덱스 등을 활용하여 보다 정확하게 부분 문자열을 추출하는 방법도 있습니다.

## 더 알아보기

문자열에서 부분 문자열을 추출하는 방법에 대해 더 알아보려면 다음 링크들을 참고해보세요.

- [Haskell 문자열 함수 공식 문서](https://hackage.haskell.org/package/base-4.15.0.0/docs/Data-String.html)
- [Haskell 문자열 처리 관련 예시 코드](https://wiki.haskell.org/Strings)
- [Haskell 문자열 처리에 대한 블로그 포스트](https://mmhaskell.com/blog/2020/3/14/haskell-string-types-and-manipulation)