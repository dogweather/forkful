---
title:                "Haskell: 패턴에 일치하는 문자 삭제하기"
programming_language: "Haskell"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/haskell/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## 왜

문자 패턴을 일치하는 문자를 삭제하는 작업에 착수해야 하는 이유는 무엇일까요?

## 방법

Haskell에서 패턴 매칭을 사용하여 문자를 삭제하는 방법은 간단합니다. 코드 블록에서 예제와 출력을 확인해보세요.

```Haskell
import Text.Regex.Posix (sub)

-- "hello world" 문자열에서 모음을 삭제하는 예제
sub "[aeiou]" "hello world" ""
```

이 코드를 실행하면 "hll wrld"라는 출력을 얻을 수 있습니다. 위 코드에서 사용된 정규식에서 [aeiou]는 삭제하고자 하는 모음을 의미합니다. 여러분은 이를 원하는대로 수정하여 원하는 문자를 삭제할 수 있습니다.

따라서 Haskell의 간단한 패턴 매칭을 이용하면 여러분이 원하는 패턴의 문자를 쉽게 삭제할 수 있습니다.

## 심층 분석

문자를 삭제하는 작업은 문자열 처리를 할 때 매우 유용합니다. 문자를 삭제하면 원하는 패턴에 대한 데이터를 깔끔하게 정제할 수 있습니다. 또한 정규식을 사용하여 광범위한 패턴을 삭제할 수 있기 때문에 매우 유연하게 사용할 수 있는 기능입니다.

그러나 문자를 삭제하는 작업이 곧바로 필요한 경우는 많지 않기 때문에 유효한 이유가 있을 때에만 사용하는 것이 좋습니다. 만약 여러분이 문자열 처리를 하다가 정제가 필요하다고 느낀다면 문자를 삭제하는 방법을 고려해보세요.

## 참고 자료

- [Haskell.org](https://www.haskell.org/)
- [W3Schools - Regular Expressions](https://www.w3schools.com/python/python_regex.asp)
- [Hackage - Text.Regex.Posix](https://hackage.haskell.org/package/regex-posix)
- [Learn You a Haskell - Regular Expressions](http://learnyouahaskell.com/starting-out#regular-expressions)