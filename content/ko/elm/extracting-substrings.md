---
title:    "Elm: 서브스트링 추출"
keywords: ["Elm"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/ko/elm/extracting-substrings.md"
---

{{< edit_this_page >}}

## 왜

우리는 프로그래밍을 할 때 자주 문자열에서 일부를 추출하는 경우가 있습니다. 이는 특정 데이터를 찾거나 특정 형식의 문자열을 만들기 위해서일 수 있습니다. Elm에서는 `String` 모듈을 사용하여 손쉽게 문자열에서 원하는 부분을 추출할 수 있습니다.

## 추출하는 방법

Elm에서는 `String` 모듈의 `slice` 함수를 사용하여 문자열에서 일부를 추출할 수 있습니다. 예를 들어, 다음과 같이 사용할 수 있습니다:

```Elm
name = "John Smith"
firstName = String.slice 0 4 name
```

위의 예시에서 `String.slice 0 4 name`는 `"John"`이라는 결과를 반환합니다. 첫 번째 매개변수는 추출하고 싶은 부분의 첫 번째 인덱스를 나타내고, 두 번째 매개변수는 추출하고 싶은 부분의 마지막 인덱스를 나타냅니다. 그리고 세 번째 매개변수는 추출하고 싶은 문자열입니다.

또 다른 예시를 살펴보겠습니다:

```Elm
phoneNumber = "123-456-7890"
lastFourDigits = String.slice 8 11 phoneNumber
```

위의 예시에서 `String.slice 8 11 phoneNumber`는 `"7890"`이라는 결과를 반환합니다. 이와 같은 방식으로 원하는 부분을 추출할 수 있습니다.

## 깊게 파보기

`String` 모듈에는 `slice` 함수외에도 다양한 함수가 있습니다. 예를 들어 `substring` 함수는 `slice`와 비슷하지만 첫 번째 매개변수와 두 번째 매개변수의 순서가 반대입니다. 그리고 `left`와 `right` 함수는 주어진 문자열에서 왼쪽이나 오른쪽 일부를 추출하는 함수입니다. 이 외에도 `filter` 함수를 사용하여 말 그대로 문자열에서 특정 문자를 필터링할 수 있습니다.

이러한 함수들은 우리의 작업을 더욱 쉽게 만들어주는 유용한 기능들입니다. 자세한 내용은 [Elm 공식 문서](https://package.elm-lang.org/packages/elm/core/latest/String)를 참고해주세요.

## 더보기

- [Elm 문자열 함수들에 대한 자세한 설명 (번역)](https://elmbyexample.github.io/translation/string-operations/)
- [Elm 문자열 관련 실제 사용 사례 예제](https://thoughtbot.com/blog/elm-strings)