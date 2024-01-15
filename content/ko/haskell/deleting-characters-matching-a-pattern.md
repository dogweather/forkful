---
title:                "패턴과 일치하는 문자 삭제하기"
html_title:           "Haskell: 패턴과 일치하는 문자 삭제하기"
simple_title:         "패턴과 일치하는 문자 삭제하기"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/haskell/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## 왜 지우려는가?
패턴과 일치하는 문자를 지우는 것의 이점은 불필요한 정보를 제거하여 코드를 더 간결하고 읽기 쉽게 만드는 것입니다. 또한, 특정 조건에 따라 문자를 지우는 방법이 필요할 수 있습니다.

## 지우는 방법
### 단일 문자 지우기
```Haskell
import Data.List (delete)

delete 'a' "Haskell" -- "Hskell"
delete 'a' "banana" -- "bnana"
```

### 패턴과 일치하는 모든 문자 지우기
```Haskell
import Data.List (delete)
import Data.Char (isDigit)

deleteIfDigit = delete `whereIsDigit` getDigits
    where
        whereIsDigit x = if isDigit x then True else False
        getDigits = "Haskell123" -- "Haskell"
```

## 깊은 탐구
문자열에서 특정 패턴의 문자를 지우는 방법은 `Data.List` 모듈의 `delete` 함수를 사용하여 간단하게 처리할 수 있습니다. 또한 조건에 따라 문자를 지우는 경우에는 일반적인 함수를 사용할 수 있지만, 중간 함수를 정의하여 더욱 간결한 코드를 작성할 수도 있습니다.

## 참고 자료
[Official Haskell Documentation](https://www.haskell.org/documentation/) <br>
[Tutorialspoint Haskell Tutorial](https://www.tutorialspoint.com/haskell/index.htm) <br>
[Hackage: Data.List Module](https://hackage.haskell.org/package/base-4.14.0.0/docs/Data-List.html)