---
title:                "Haskell: 정규 표현식 사용하기"
simple_title:         "정규 표현식 사용하기"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/haskell/using-regular-expressions.md"
---

{{< edit_this_page >}}

# 왜 정규 표현식을 사용할까요?

정규 표현식은 문자열을 검색하고 일치하는 패턴을 찾는 데 유용한 도구입니다. 이를 사용하면 문자열에서 원하는 정보를 추출하고, 데이터를 처리하거나 변환하는 데 효율적으로 사용할 수 있습니다.

# 사용 방법

먼저, 정규 표현식을 사용하기 위해 `regex` 라이브러리를 불러와야 합니다. 그 다음, 해당 문자열과 패턴을 `=~` 연산자로 비교하여 일치 여부를 확인할 수 있습니다.

```Haskell
import Text.Regex.Posix

-- 패턴 설정
pattern = "[0-9]+"

-- 비교할 문자열
string = "Hello123"

-- 비교 및 일치 여부 확인
string =~ pattern
-- 결과: True
```

정규 표현식에서 정수만 추출하고 싶다면, `=~` 연산자가 True를 반환하는 경우 `match` 함수를 사용해 일치하는 부분을 추출할 수 있습니다.

```Haskell
-- 패턴 설정
pattern = "[0-9]+"

-- 비교할 문자열
string = "Hello123"

-- 일치하는 부분 추출
let match = string =~ pattern :: (String, String, String)
-- 결과: ("Hello", "123", "")
-- 첫 번째 요소는 일치하는 부분 이전의 문자열, 두 번째 요소는 일치하는 부분 자체, 세 번째 요소는 일치하는 부분 이후의 문자열을 나타냅니다.
```

# 더 깊게 알아보기

정규 표현식에서 사용하는 특수문자에는 `.`, `+`, `*`, `?` 등이 있습니다. 이들은 해당 위치에 어떤 문자가 오더라도 일치하도록 하는 기능을 합니다. 또한 `|` 기호를 사용하여 여러 개의 패턴을 포함할 수도 있습니다.

정규 표현식을 활용하면 더 복잡한 패턴을 만들 수도 있습니다. 예를 들어, 이메일 주소를 검증할 때 사용하는 패턴은 다음과 같이 작성할 수 있습니다.

```Haskell
-- 패턴 설정
pattern = "[A-Za-z0-9]+@[A-Za-z0-9]+\\.[A-Za-z]+"

-- 비교할 문자열
string = "abc123@test.com"

-- 비교 및 일치 여부 확인
string =~ pattern
-- 결과: True
```

하지만 정규 표현식은 간단한 패턴부터 더 복잡한 패턴까지 다양하게 활용할 수 있기 때문에 또다른 예제를 통해 더 많은 기능을 알아보기를 추천합니다.

# 더 알아보기

## 참고 자료

- [Haskell regex 라이브러리 문서](http://hackage.haskell.org/package/regex)
- [정규 표현식 실습 사이트](https://regexr.com/)
- [정규 표현식 강좌 비디오](https://www.youtube.com/watch?v=r6I-Ahc0HB4)

## 관련 블로그 포스트

- [Haskell에서 정규 표현식 활용하기](https://key not found 이후 URL로 변경하기)