---
title:                "패턴과 일치하는 문자 삭제하기"
html_title:           "Elm: 패턴과 일치하는 문자 삭제하기"
simple_title:         "패턴과 일치하는 문자 삭제하기"
programming_language: "Elm"
category:             "Elm"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/elm/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## 왜

여러분은 유용한 도구이자 함수의 하나인 패턴을 매칭하여 문자를 삭제하는 것이 왜 유용한지 궁금해 할 수 있습니다. 이를 통해 불필요한 정보나 데이터를 간단하고 효율적으로 제거할 수 있기 때문입니다.

## 어떻게

```Elm
-- 문자열에서 모음을 제거하는 예시
deleteVowels: String -> String
deleteVowels str =
    String.filter (\char -> not (List.member char ["a", "e", "i", "o", "u"])) str
```

위의 예시에서는 `String.filter` 함수를 사용하여 주어진 문자열에서 주어진 조건을 충족하는 문자를 제거하는 함수를 만들었습니다. 위의 예시에서는 `List.member` 함수를 사용하여 모음이 포함되어 있으면 제거하도록 조건을 설정하였습니다.

**출력:**

```Elm
deleteVowels "Hello World"
-- "Hll Wrld"
```

위의 예시에서는 `Hello World` 문자열에서 모음인 `e`와 `o`가 제거되어 `Hll Wrld`라는 결과가 나오게 됩니다. 위와 같이 간단하고 효율적으로 문자를 삭제할 수 있는 예시들을 많이 만들어 놓으면 나중에 유용하게 사용할 수 있습니다.

## 딥 다이브

패턴을 매칭하여 문자를 제거하는 것은 내부적으로 많은 과정을 거칩니다. 가장 일반적인 방법은 문자열을 순차적으로 탐색하면서 패턴을 만족하는 문자를 삭제하는 것입니다. 이 과정에서는 많은 조건문이 사용되며, 문자열의 길이가 길어질수록 더 많은 연산이 필요하게 됩니다. 따라서 패턴을 매칭하여 문자를 제거하는 것이 필요할 때는 조건에 맞는 효율적인 방법을 선택하는 것이 중요합니다.

## 참고자료

- [String - Elm Lang Documentation](https://package.elm-lang.org/packages/elm/core/latest/String)
- [Pattern Matching - Elm Lang Documentation](https://guide.elm-lang.org/types/union_types.html#pattern-matching)