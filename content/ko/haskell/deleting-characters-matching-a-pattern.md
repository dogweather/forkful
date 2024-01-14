---
title:                "Haskell: 패턴과 일치하는 문자 삭제하기"
simple_title:         "패턴과 일치하는 문자 삭제하기"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/haskell/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## 왜
이번 블로그에서는 패턴과 일치하는 문자를 삭제하는 것이 왜 중요한지 설명하고자 한다. 패턴 매칭은 Haskell 프로그래밍에서 매우 유용한 기능 중 하나이며, 패턴을 삭제하는 것은 전체 코드의 가독성을 향상시키고 작업을 더 효율적으로 만들어준다.

## 하는 방법
먼저, 패턴 매칭을 사용하기 위해 `import Text.Regex.Posix`를 통해 모듈을 불러와야 한다. 다음은 패턴과 일치하는 문자를 삭제하는 코드의 예제이다.

```Haskell
deletePattern :: String -> String
deletePattern input = subRegex (mkRegex "[aeiou]") input ""
```

위의 코드에서 `deletePattern` 함수는 매개변수로 문자열을 받아 패턴과 일치하는 문자가 삭제된 문자열을 반환한다. 이 함수는 `subRegex` 함수를 사용하고 있으며, 첫 번째 매개변수는 패턴을 나타내고 두 번째 매개변수는 대상 문자열을 나타낸다. 마지막 매개변수는 대체할 문자열이며, 위의 예제에서는 빈 문자열을 지정하여 문자를 삭제하도록 한다.

다음은 위의 코드를 실행한 결과이다. 

```Haskell
deletePattern "Hello, world!" = "Hll, wrld!"
```

위의 예제에서 보듯이, 패턴과 일치하는 모든 문자가 삭제되었음을 알 수 있다.

## 심화 분석
패턴 매칭은 Haskell에서 매우 강력한 기능 중 하나이다. 마스크 문자를 사용하여 자리수를 지정할 수 있고, 사용된 문자열을 다양한 방식으로 변환할 수 있다는 점에서 유용하게 사용될 수 있다. 패턴을 통해 여러 가지 작업을 수행할 수 있다는 것을 알고 있으면 코드 작성에 있어서 더 유연하게 할 수 있게 될 것이다.

## 관련 항목

- [Learn You a Haskell](http://learnyouahaskell.com/chapters)
- [Haskell Wiki](https://www.haskell.org/haskellwiki/Haskell)
- [Real World Haskell](http://book.realworldhaskell.org/read/)