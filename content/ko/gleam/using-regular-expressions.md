---
title:                "정규식 사용하기"
html_title:           "Gleam: 정규식 사용하기"
simple_title:         "정규식 사용하기"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/gleam/using-regular-expressions.md"
---

{{< edit_this_page >}}

## 왜
컴퓨터 프로그래밍을 하는 많은 사람들에게 정규 표현식을 사용하는 것은 중요합니다. 정규 표현식은 텍스트 데이터를 처리할 때 매우 강력한 도구이기 때문입니다. 정규 표현식을 사용하면 복잡한 문자열 패턴을 일치시키고 추출할 수 있으며, 데이터를 변환하고 찾을 수 있습니다.

## 어떻게
정규 표현식을 사용하기 전에, 먼저 ```Gleam.Regex``` 모듈을 임포트해야 합니다. 이 모듈은 정규 표현식을 만들고 처리하는 함수들을 제공합니다. 예를 들어, "a"와 "b" 사이에 있는 문자열을 추출하는 코드는 다음과 같이 작성할 수 있습니다.

```Gleam
import Gleam.Regex

let string = "abc"
let regex = Regex.new("a(.)b")
let matches = Regex.find_all(regex, string)

assert Ok([("b", ["c"])]) == matches
```

위의 예제에서는 "abc" 문자열에서 "b"와 "c"를 추출하여 "a"와 "b" 사이에 있는 문자열 패턴을 일치시키는 것을 보여줍니다. 이를테면, 이러한 코드 패턴을 사용하여 데이터에 원하는 패턴을 추출하고 변환할 수 있습니다.

## 딥 다이브
정규 표현식을 사용하면 더 복잡한 문자열 패턴을 일치시킬 수 있습니다. 예를 들어, 전화번호를 일치시키는 패턴을 만들어보겠습니다. 전화번호는 일반적으로 "010-1234-5678" 또는 "01012345678"과 같은 형식을 가지고 있습니다. 이를 표현하는 정규 표현식은 다음과 같이 작성할 수 있습니다.

```Gleam
import Gleam.Regex

let string = "010-1234-5678"
let regex = Regex.new("([0-9]{3}-?[0-9]{4}-?[0-9]{4})")
let matches = Regex.find_all(regex, string)

assert Ok([("010-1234-5678", ["010-1234-5678"])]) == matches
```

위의 예제에서는 전화번호가 "010-1234-5678" 형식에 있는 경우 일치하도록 정규 표현식을 작성했습니다. 이처럼 정규 표현식을 사용하면 다양한 문자열 패턴을 일치시킬 수 있습니다.

## 참고
- Gleam 공식 문서 (https://gleam.run)