---
title:                "문자열 연결"
html_title:           "Elm: 문자열 연결"
simple_title:         "문자열 연결"
programming_language: "Elm"
category:             "Elm"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/elm/concatenating-strings.md"
---

{{< edit_this_page >}}

## 무엇 & 왜?
문자열 연결(concatenation)이란 무엇인지 알고 싶으신가요? 그럼 이 글을 읽어보세요! 문자열 연결은 두 문자열을 합쳐서 새로운 하나의 문자열로 만드는 것을 말합니다. 프로그래머들은 주로 문자열 연결을 사용하여 더 복잡한 문자열을 만들거나 변수에 저장된 값을 문자열로 변환할 때 사용합니다.

## 어떻게:
문자열 연결을 한 줄로 간단하게 할 수 있어요! 
```Elm
let name = "Alice"
let greeting = "Hello"

let message = greeting ++ " " ++ name  //output: "Hello Alice"
```

또는 여러 줄에 나눠서 할 수도 있어요.
```Elm
let sentence = "This is a" ++ 
               "long sentence" ++ 
               "that needs to be" ++
               "concatenated." //output: "This is a long sentence that needs to be concatenated."
```

## 깊이 파고들기:
(1) 역사적 맥락: 문자열 연결은 컴퓨터 과학에서 오래된 개념이에요. 처음에는 이를 위해 사용하는 별도의 함수가 없었기 때문에 프로그래머들은 더 복잡한 방법으로 문자열을 합치기도 했어요. 하지만 지금은 다양한 언어에서 문자열 연결을 지원하기 때문에 한 줄로 간단하게 할 수 있답니다.

(2) 대안: 더 많은 문자열 처리 옵션을 제공하는 언어도 있지만 대부분의 프로그래머들은 문자열 연결을 택하고 있어요.

(3) 구현 세부사항: Elm에서는 문자열 연결 연산자 ```++```를 제공하며, 뒤에 오는 문자열을 앞에 오는 문자열 뒤에 붙여줍니다. 

## 관련 자료:
- Elm 공식 문서: https://guide.elm-lang.org/appendix/syntax.html
- 문자열 연결 예제: https://elmprogramming.com/string-manipulation.html#concatenation