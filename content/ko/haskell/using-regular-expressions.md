---
title:                "정규식을 이용한 컴퓨터 프로그래밍"
html_title:           "Haskell: 정규식을 이용한 컴퓨터 프로그래밍"
simple_title:         "정규식을 이용한 컴퓨터 프로그래밍"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/haskell/using-regular-expressions.md"
---

{{< edit_this_page >}}

왜: 정규 표현식을 사용하는 이유를 최대 2문장으로 설명합니다.

개발자들은 여러 문자열을 다루고 싶을 때, 검색이나 교체, 유효성 검사 등의 작업을 간편하게 수행하기 위해 정규 표현식을 사용합니다. 이를 통해 코드의 가독성을 높이고 작업을 보다 효율적으로 처리할 수 있기 때문입니다.

## 사용법

Haskell에서 정규 표현식을 사용하기 위해서는 `Text.Regex.PCRE` 모듈을 임포트해야 합니다.

```Haskell
import Text.Regex.PCRE
```

### 검색하기

`Text.Regex.PCRE` 모듈의 `=~` 연산자를 이용하여 문자열에서 패턴에 맞는 부분을 찾을 수 있습니다.

```Haskell
"Hello, World" =~ "Hello" :: Bool   -- True
"12345" =~ "\\d+" :: Bool   -- True (숫자 하나 이상인 패턴에 일치)
```

`=~` 연산자의 두 번째 인자는 정규 표현식이어야 하며, 이를 기반으로 첫 번째 인자인 문자열에서 일치하는 부분을 찾습니다. `:: Bool`은 결과 값의 유형을 명시해주기 위해 사용되는 코드이며, 일치하면 `True`를 반환합니다.

### 교체하기

`Text.Regex.PCRE` 모듈의 `subRegex` 함수를 이용하면 문자열에서 패턴에 맞는 부분을 원하는 형식으로 교체할 수 있습니다.

```Haskell
subRegex (mkRegex "\\d+") "I have 5 apples" "five"
-- "I have five apples"
```

`subRegex` 함수의 첫 번째 인자는 교체할 문자열입니다. 두 번째 인자는 교체할 패턴이며, `mkRegex` 함수를 사용하여 문자열을 정규 표현식으로 변환해야 합니다. 세 번째 인자는 교체할 문자열로 대체됩니다.

### 유효성 검사하기

`Text.Regex.PCRE` 모듈의 `matchRegex` 함수를 이용하면 문자열이 정해진 패턴에 일치하는지 검사할 수 있습니다.

```Haskell
matchRegex (mkRegex "^hello") "Hello, World"
-- Nothing (일치하지 않으므로 Nothing 반환)
```

일치하는 부분이 없으면 `Nothing`을 반환하며, 일치하는 부분이 있으면 `Just`를 사용하여 해당 부분을 포함하는 리스트를 반환합니다.

## 깊이 살펴보기

정규 표현식은 문자열에 대한 패턴을 정의하는 것으로 매우 강력하고 유연한 기능을 제공합니다. 하지만 정규 표현식을 작성하는 것은 단순하지 않을 수 있습니다. 따라서 정규 표현식을 작성할 때는 다음과 같은 사항을 고려해야 합니다.

- 패턴의 중복 최소화: 같은 패턴이 여러 번 반복되는 것을 최대한 피하는 것이 최적화에 도움이 됩니다.
- 재귀적 패턴 활용: 재귀적 패턴을 이용하면 패턴의 복잡도를 줄일 수 있습니다.
- 문자 클래스 사용: 문자 클래스는 많은 문자들 중에서 일치하는 것을 찾는데 도움이 됩니다.
- 그룹화: 패턴의 일부를 그룹화하여 특정 부분을 더 쉽게 추출할 수 있습니다.

## 참고 자료

- [Haskell Documentation - Text.Regex.PCRE Module](https://hackage.haskell.org/package/