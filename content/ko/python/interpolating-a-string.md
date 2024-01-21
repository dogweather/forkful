---
title:                "문자열 보간하기"
date:                  2024-01-20T17:51:26.935178-07:00
model:                 gpt-4-1106-preview
simple_title:         "문자열 보간하기"
programming_language: "Python"
category:             "Python"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/python/interpolating-a-string.md"
---

{{< edit_this_page >}}

## What & Why? (무엇과 왜?)
문자열 보간은 변수나 표현식의 값을 문자열 내에 넣는 것입니다. 프로그래머들은 동적으로 메시지를 생성하거나 코드의 가독성을 향상시키기 위해 이를 사용합니다.

## How to (방법)
```Python
# 새로운 방식: f-string을 사용한 문자열 보간
name = "Jin"
age = 25
greeting = f"안녕, 내 이름은 {name}이고, 나이는 {age}살이야."
print(greeting)

# 출력: 안녕, 내 이름은 Jin이고, 나이는 25살이야.

# 구 방식: format 메소드 사용
greeting = "안녕, 내 이름은 {}이고, 나이는 {}살이야.".format(name, age)
print(greeting)

# 출력: 안녕, 내 이름은 Jin이고, 나이는 25살이야.
```

## Deep Dive (심층 분석)
초기에는 `%` 연산자를 사용한 문자열 포매팅이 흔했습니다. 이후 `.format()` 메소드가 나타났고, Python 3.6부터는 f-string이라는 더 효과적인 방법이 도입되었습니다. 이는 컴파일 시간에 문자열이 처리되어 실행 시간에는 변환된 문자열이 사용된다는 장점이 있습니다.

대체로, f-string은 `str.format()`이나 `%` 연산자에 비해 더 빠르고 읽기 쉬운 코드를 제공합니다. 성능이 중요하거나 매우 적은 메모리를 사용해야 하는 상황에서는 f-string이 유용합니다.

모든 Python 표현식을 `{}` 안에 넣어 동적으로 평가할 수 있다는 것도 기억할 만한 점입니다. 그러나 보안 문제 때문에 사용자 입력을 직접 f-string 내에 넣는 것은 피해야 합니다.

## See Also (관련 자료)
- [Python 공식 문서](https://docs.python.org/3/reference/lexical_analysis.html#f-strings)에서 f-string에 대해 더 자세히 알아볼 수 있습니다.
- [PEP 498](https://www.python.org/dev/peps/pep-0498/) — f-string이 소개된 Python Enhancement Proposal 문서입니다.
- [Real Python Tutorial](https://realpython.com/python-f-strings/)에서 문자열 보간 사용법에 대한 더 많은 예제를 찾아볼 수 있습니다.