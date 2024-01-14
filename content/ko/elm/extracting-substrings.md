---
title:                "Elm: 부분 문자열 추출하기"
programming_language: "Elm"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/elm/extracting-substrings.md"
---

{{< edit_this_page >}}

# 왜 substring 추출을 해야할까요?

Substring 추출은 Elm 프로그래밍에서 문자열을 다룰 때 매우 유용한 기능입니다. 이 기능을 사용하면 문자열 데이터에서 원하는 부분만을 추출하여 간편하게 다룰 수 있습니다.

## 방법

다음은 substring 추출을 하는 방법을 보여주는 예제 코드와 출력입니다. 먼저 문자열 데이터 "안녕하세요"를 정의합니다.

```Elm
string = "안녕하세요"
```

그리고 substring을 추출하기 위해 Elm의 `String.slice` 함수를 사용합니다. 이 함수는 총 세 개의 인자를 받습니다. 첫 번째 인자는 문자열 데이터에서 추출을 시작할 인덱스, 두 번째 인자는 추출을 끝낼 인덱스, 세 번째 인자는 원하는 문자열 데이터를 전달합니다. 예를 들어서, "안녕하세요"에서 "녕하"만을 추출하고 싶다면 다음과 같이 코드를 작성할 수 있습니다.

```Elm
substring = String.slice 1 3 string
```

다음은 출력 결과입니다.

```Elm
"녕하"
```

위의 예제에서 볼 수 있듯이, `String.slice` 함수를 사용하면 간단하게 substring을 추출할 수 있습니다.

## 더 깊게 들어가기

Elm 문자열 데이터에서 substring을 추출할 때 고려해야 할 몇 가지 사항이 있습니다. 첫 번째로, 인덱스는 0부터 시작한다는 점입니다. 따라서 위의 예제에서 추출을 시작하는 인덱스는 실제 문자열 데이터에서는 2번째 글자인 "녕"이지만, 코드에서는 1로 전달해야 합니다. 두 번째로, 추출을 끝내는 인덱스는 실제 문자열 데이터에서는 4번째 글자인 "하"가 아니라 3으로 전달해야 합니다. 마지막으로, 인덱스는 항상 문자열 데이터의 길이보다 작아야 합니다.

예를 들어서, 위의 예제에서 우리가 끝 인덱스를 4로 전달하면 에러가 발생합니다. 위의 예제에서는 문자열 데이터의 길이가 4보다 작기 때문에 에러가 발생하지 않지만, 만약 문자열 데이터가 "안녕해요"라면 끝 인덱스는 5가 아닌 4로 전달해야 합니다.

## 더 알아보기

substring 추출과 관련된 더 자세한 내용을 알고 싶다면, Elm 공식 문서를 참고해보세요. 또한, 아래의 자료들도 유용할 것입니다.

- Elm 공식 문서: https://guide.elm-lang.org/strings.html#slices
- Elm 문자열 데이터 다루기: https://elmprogramming.com/strings.html
- 문자열 추출과 관련된 다양한 예제: https://elmprogramming.com

# 관련 자료

- Elm 문자열 데이터 다루기: https://elmprogramming.com/strings.html
- Elm 문자열 데이터 소개: https://elmprogramming.com/a-string-introduction.html