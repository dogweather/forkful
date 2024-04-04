---
changelog:
- 2024-04-04, dogweather, edited
- 2024-04-04, gpt-4-0125-preview, translated from English
date: 2024-01-20 17:43:02.363431-07:00
description: "\uBC29\uBC95: ."
lastmod: '2024-04-04T01:28:16.279767-06:00'
model: gpt-4-0125-preview
summary: .
title: "\uD328\uD134\uC5D0 \uC77C\uCE58\uD558\uB294 \uBB38\uC790 \uC0AD\uC81C\uD558\
  \uAE30"
weight: 5
---

## 방법:
```Python
import re

# 예제 문자열
text = "Hello, World! 1234"

# 모든 숫자 제거
no_digits = re.sub(r'\d', '', text)
print(no_digits)  # 출력: "Hello, World! "

# 구두점 제거
no_punctuation = re.sub(r'[^\w\s]', '', text)
print(no_punctuation)  # 출력: "Hello World 1234"

# 모음 제거
no_vowels = re.sub(r'[aeiouAEIOU]', '', text)
print(no_vowels)  # 출력: "Hll, Wrld! 1234"
```

### 제가 작성한 맞춤형 함수

이 작업을 자주 수행하기 때문에 `delete()` 함수로 리팩토링했습니다. [doctests](https://docs.python.org/3/library/doctest.html)의 좋은 시연이기도 합니다:

```python
def delete(string: str, regex: str) -> str:
    """
    >>> delete("Hello, world!", "l")
    'Heo, word!'

    >>> delete("Hello, world!", "[a-z]")
    'H, !'
    """
    return re.sub(regex, "", string)
```



## 심층 분석
텍스트에서 패턴과 일치하는 문자를 삭제하는 작업은 컴퓨터 과학에서 깊은 뿌리를 가지고 있으며, `sed`와 `grep` 같은 초기 Unix 도구로 거슬러 올라갑니다. Python에서는 `re` 모듈이 이 기능을 제공하며, 정규 표현식을 사용하여 텍스트 처리에 매우 강력하고 다재다능한 도구를 제공합니다.

`re` 모듈의 대안으로는 다음이 있습니다:
- 간단한 경우를 위한 문자열 메서드 `replace()`.
- 더 복잡한 패턴과 더 나은 유니코드 지원을 위한 타사 라이브러리 `regex`.

내부적으로, `re.sub()`를 사용할 때, Python 인터프리터는 패턴을 바이트코드 시리즈로 컴파일하고, 입력 텍스트에 직접 패턴 매칭을 수행하는 상태 기계에 의해 처리됩니다. 이 작업은 큰 문자열이나 복잡한 패턴에 대해 자원 집약적일 수 있으므로, 대용량 데이터 처리에 대한 성능 고려사항이 중요합니다.

## 참조
- [Python `re` 모듈 문서](https://docs.python.org/3/library/re.html): Python에서 정규 표현식에 대한 공식 문서.
- [Regular-Expressions.info](https://www.regular-expressions.info/): 정규 표현식에 대한 포괄적인 가이드.
- [Real Python의 정규 표현식 튜토리얼](https://realpython.com/regex-python/): Python에서 정규 표현식의 실제 응용.
