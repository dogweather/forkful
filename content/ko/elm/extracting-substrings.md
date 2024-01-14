---
title:    "Elm: 문자열 추출하기"
keywords: ["Elm"]
---

{{< edit_this_page >}}

## 왜
우리는 프로그래밍을 할 때 종종 문자열에서 일부분을 추출해야 할 때가 있습니다. 예를 들면, 사용자의 이름을 입력 받고 그 중에서 성만 따로 따로 사용하고 싶을 수 있습니다. 이를 해결하기 위해 우리는 Elm에서 substring을 추출하는 방법을 배울 것입니다.

## 어떻게 
```Elm
-- 문자열에서 특정 길이 만큼의 substring 추출하기
substring : Int -> Int -> String -> String

-- 예시: "켄트법 (Kent Beck)"에서 "켄트"만 추출하기
substring 0 4 "켄트법 (Kent Beck)"

-- 결과: "켄트"

-- 문자열에서 특정 문자열 이후의 모든 문자 추출하기
dropLeft : Int -> String -> String

-- 예시: " Elm은 멋진 프로그래밍 언어입니다."에서 " Elm은" 제거하기
dropLeft 17 " Elm은 멋진 프로그래밍 언어입니다."

-- 결과: " 멋진 프로그래밍 언어입니다."
```

## 깊이 파헤치기
substring과 dropLeft는 우리가 흔히 쓰는 기능이지만 실제로는 인덱스를 계산하고 그에 맞게 문자열을 추출하는 일련의 과정을 거칩니다. 이러한 과정 덕분에 우리는 문자열에서 자유롭게 원하는 부분만 추출해 낼 수 있습니다. 하지만, 인덱스를 계산하는 것은 종종 복잡한 작업일 수 있기 때문에 substring과 dropLeft를 사용하기 전에 해당 문자열을 잘 확인하고 어디서부터 어디까지의 인덱스가 필요한지 파악하는 것이 중요합니다.

## 참고 자료
- Elm 문자열 라이브러리 공식 문서: https://package.elm-lang.org/packages/elm/core/latest/String
- Elm 코딩 도구 및 커뮤니티: https://elm-lang.org/
- Elm을 사용한 개발 블로그: https://thoughtbot.com/blog/using-elm-is-happy-fun