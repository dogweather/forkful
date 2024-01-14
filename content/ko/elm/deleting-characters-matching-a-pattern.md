---
title:    "Elm: 패턴과 일치하는 문자 삭제하기"
keywords: ["Elm"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/ko/elm/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## 왜

문자 매칭 패턴을 지우는 작업을 진행하는 이유는 무엇일까요? 대부분의 경우, 우리는 원하지 않는 문자열을 제거하거나 수정하기 위해 이 작업을 수행합니다.

## 하는 방법

다음은 문자 매칭 패턴을 지우는 방법에 대한 간단한 예제 코드입니다.

```Elm
import String

inputString = "Hello, world!"
pattern = "H" -- 지워야 하는 문자 패턴

result = String.replace pattern "" inputString -- "ello, world!"
```

위의 예제에서 우리는 `String.replace` 함수를 사용하여 문자 매칭 패턴을 지우는 것을 볼 수 있습니다. 이 함수는 입력 문자열에서 지정된 패턴에 해당하는 모든 문자를 빈 문자열 ""로 대체합니다.

더 많은 예제를 살펴보기 전에, 먼저 문자 매칭 패턴을 지우는데 사용할 수 있는 세 가지 함수를 알아보겠습니다.

* `String.replace` - 입력 문자열에서 지정된 패턴에 일치하는 모든 문자를 대체합니다.
* `String.dropLeft` - 입력 문자열에서 왼쪽에서부터 지정된 수만큼 문자를 제거합니다.
* `String.dropRight` - 입력 문자열에서 오른쪽에서부터 지정된 수만큼 문자를 제거합니다.

이제 각 함수를 사용하는 예제를 살펴보겠습니다.

```Elm
import String

-- "String"이라는 단어를 제외한 모든 문자를 지웁니다.
pattern = "String"
inputString = "This is a String example."

result = String.replace pattern "" inputString -- " is a  example."

-- 왼쪽에 있는 7개의 문자를 제거합니다.
charactersToRemove = 7
modifiedString = String.dropLeft charactersToRemove inputString -- "a String example."

-- 오른쪽에 있는 8개의 문자를 제거합니다.
charactersToRemove = 8
modifiedString = String.dropRight charactersToRemove inputString -- "This is a St"

```

이와 같이 우리는 간단하게 문자 매칭 패턴을 지우는 방법을 알아보았습니다. 하지만 더 복잡한 패턴을 다루고 싶다면 어떻게 해야 할까요? 이제 심화 공부를 해보겠습니다.

## 심화 공부

더 복잡한 문자 매칭 패턴을 다루기 위해서는 정규식(Regular Expressions)이라는 개념을 사용해야 합니다. 정규식은 특정 패턴을 가진 문자열을 매칭시켜주는 형식 언어입니다.

예를 들어, 우리가 문자열에서 모든 숫자를 제거하고 싶다면 어떻게 할까요? 정규식을 사용하면 간단하게 해결할 수 있습니다.

```Elm
import RegExp

inputString = "1a2b3c"
pattern = RegExp.fromString "[0-9]" -- 0부터 9까지의 숫자를 매칭하는 정규식

result = RegExp.replace pattern "" inputString -- "abc"
```

이와 같이 정규식을 사용하면 더 복잡한 패턴을 처리할 수 있습니다. 하지만 정규식은 익숙하지 않은 개념일 수 있으므로, 모든 상황에서 정규식을 사용하는 것은 필수적이지 않습니다. 간단한 문자 매칭 패턴을 처리하기 위해 Elm의 내장 함수들을 활용하는 것만으로도 충분히 가능합니다.

## 더 알아보기

* [Elm 공식 문서 - String 모듈](https://guide.elm-lang