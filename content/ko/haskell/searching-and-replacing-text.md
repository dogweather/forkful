---
title:                "텍스트 검색 및 바꾸기"
html_title:           "Haskell: 텍스트 검색 및 바꾸기"
simple_title:         "텍스트 검색 및 바꾸기"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/haskell/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## 무엇 & 왜?
검색 및 텍스트 교체란 무엇이며, 왜 프로그래머들이 이를 수행하는지에 대해 2-3문장으로 설명하겠습니다.

검색과 교체는 텍스트를 찾고 원하는 텍스트로 대체하는 작업입니다. 이 작업은 코드 수정이나 데이터 정리 등 다양한 작업에 이용될 수 있습니다.

## 방법:
아래의 ```Haskell ... ``` 코드 블록을 이용하여 코딩 예제와 샘플 출력을 보여드리겠습니다.

### 검색하기
```Haskell
-- "hello"라는 문자열을 "안녕하세요"로 대체
replace "hello" "안녕하세요" "hello world" -- "안녕하세요 world"
```

### 정규표현식 활용하여 검색하기
```Haskell
-- 정규표현식을 이용하여 이메일 주소 형식의 문자열을 찾음
search "^[A-Za-z0-9._%+-]+@[A-Za-z0-9.-]+\\.[A-Za-z]{2,15}$" "My email is example@gmail.com" -- "example@gmail.com"
```

### 대소문자 무시하여 검색하기
```Haskell
-- "hello" 또는 "HELLO"를 "안녕"으로 대체
replaceIgnoreCase ["hello", "HELLO"] "안녕" "Hello World" -- "안녕 World"
```

## 딥 다이브:
검색하고 교체하는 작업은 프로그래밍에서 필수적인 작업입니다. 하지만 쓰이는 언어나 라이브러리마다 구현 방식이 다르기 때문에 해당 언어나 라이브러리의 문서를 참조하는 것이 중요합니다.

또한 검색 및 교체 작업은 문자열을 다루는 번거로운 작업을 대신할 수 있도록 정규표현식과 같은 대안들이 제공되고 있습니다.

## 추가 자료:
검색 및 교체 작업과 관련된 자료를 아래 링크에서 확인할 수 있습니다.

[Hackage - Text Search and Replace](https://hackage.haskell.org/package/searchandreplace)