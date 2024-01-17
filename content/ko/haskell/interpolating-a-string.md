---
title:                "문자열 보간 (Transliteration: Munja-ryeol Bo-gan)"
html_title:           "Haskell: 문자열 보간 (Transliteration: Munja-ryeol Bo-gan)"
simple_title:         "문자열 보간 (Transliteration: Munja-ryeol Bo-gan)"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/haskell/interpolating-a-string.md"
---

{{< edit_this_page >}}

## 뭐고 왜하는가?
문자열 내삽(interpolating a string)이란 무엇일까요? 그리고 프로그래머들이 왜 이것을 하는 걸까요? 문자열 내삽은 문자열 내부에 변수 값을 삽입하여 조합하는 것을 의미합니다. 프로그래머들은 이를 통해 코드를 더 짧고 읽기 쉽게 만들어줍니다.

## 하는 법:
다음으로 몇 가지 예시를 보여드리겠습니다. 우선 문자열 내삽을 사용하지 않는 경우와 사용하는 경우의 차이를 보여드릴게요. 우선 아래와 같은 변수를 정의해주세요.
```Haskell
let name = "John"
let age = 27
```
그리고 이를 사용하여 문자열을 조합하는 코드를 작성해봅시다.
```Haskell
-- 문자열 내삽을 사용하지 않을 경우
print "제 이름은 " ++ name ++ "이고 나이는 " ++ age ++ "살입니다."

-- 문자열 내삽을 사용할 경우
print "제 이름은 ${name}이고 나이는 ${age}살입니다."
```
두 코드 모두 같은 결과를 출력하지만, 내삽을 사용하는 경우 더 간결하고 읽기 쉽게 작성할 수 있습니다. 

## 깊이 파헤쳐보기:
불과 몇 년 전까지만 해도 문자열 내삽은 표준 라이브러리에 포함되어 있지 않았습니다. 대신 문법적인 트릭을 사용하여 값들을 삽입해주었습니다. 하지만 현재는 ```interpolate```라는 라이브러리가 이를 위한 함수들을 제공하므로 훨씬 더 간단하게 사용할 수 있습니다. 

또한 문자열 내삽을 대체할 수 있는 다른 방법으로는 템플릿을 사용하는 것이 있습니다. 이를 위해 ```text```라는 라이브러리를 사용할 수 있으며, 이를 통해 기존 템플릿 문법을 활용할 수 있습니다.

문자열 내삽은 런타임에서 값을 조합하므로 성능상의 문제가 있을 수 있습니다. 따라서 더 복잡한 조건문이나 반복문 등의 로직을 포함한 경우에는 문자열 내삽보다는 템플릿을 사용하는 것이 더 좋은 선택일 수 있습니다.

## 관련 정보:
- [Haskell string interpolation with examples](https://medium.com/@rbahaguejr/haskell-string-interpolation-with-examples-f6b25fa8f179)
- [Hackage: interpolate](https://hackage.haskell.org/package/interpolate)
- [Hackage: text](https://hackage.haskell.org/package/text)