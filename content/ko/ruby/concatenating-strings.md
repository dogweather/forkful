---
title:                "문자열 연결하기"
html_title:           "Arduino: 문자열 연결하기"
simple_title:         "문자열 연결하기"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/ruby/concatenating-strings.md"
---

{{< edit_this_page >}}

## 무엇이며 왜? (What & Why?)

문자열 연결(concatenating strings)이란 두 개이상의 문자열을 하나로 결합하는 것을 의미합니다. 프로그래머들은 이를 사용해서 변수들, 사용자 입력, 그리고 다른 정보들을 통합하는데 사용합니다.

## 어떻게 하나요? (How to)

```Ruby
str1 = "안녕"
str2 = " 세상"
result = str1 + str2
puts result
```
출력:
```
안녕 세상
```

또는 공간을 절약하려면, `<<` 연산자 (또는 `.concat` 메소드)를 사용하세요.
```Ruby
str1 << str2
puts str1
```
출력:
```
안녕 세상
```


## 깊이 들어가며 (Deep Dive)

문자열 연결은 Ruby에서 오래된 기능이고, 프로그래머가 선택할 수 있는 많은 방법들 중 하나입니다.

`+` 연산자는 초기 버전부터 사용할 수 있었지만, 큰 문자열을 만들 때 이 방법은 처리량이 심하게 증가 할 수 있습니다.

`.concat`와 `<<` 방법은 이 문제를 해결하기 위해 도입되었습니다. 이 방법들은 원본 문자열을 변경하기 때문에 새로운 문자열을 만드는 비용을 피할 수 있습니다.

다만, 이점을 고려하여 항상 이 방법을 사용해야하는 것은 아닙니다. 가독성과 예측 가능성이 중요할 수 있기 때문에, 어떤 방법을 사용할지 결정할 때 이 점들을 트레이드 오프해야합니다.

## 참고 (See Also)

- Ruby 공식 문서 내 문자열 연결 관련 세부 정보: [http://ruby-doc.org/core-2.7.0/String.html#method-i-3C-3C](http://ruby-doc.org/core-2.7.0/String.html#method-i-3C-3C)
- 문자열 연결의 실행 시간 복잡성에 대한 설명: [http://stackoverflow.com/questions/3832385/](http://stackoverflow.com/questions/3832385/)