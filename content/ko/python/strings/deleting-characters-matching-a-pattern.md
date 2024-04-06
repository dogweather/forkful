---
changelog:
- 2024-04-04, dogweather, edited
- 2024-04-04, gpt-4-0125-preview, translated from English
date: 2024-01-20 17:43:02.363431-07:00
description: "\uBC29\uBC95: \uC774 \uC791\uC5C5\uC744 \uC790\uC8FC \uC218\uD589\uD558\
  \uAE30 \uB54C\uBB38\uC5D0 \uAC04\uB2E8\uD55C `delete()` \uD568\uC218\uB85C \uB9AC\
  \uD329\uD1A0\uB9C1\uD588\uC2B5\uB2C8\uB2E4. \uC774\uAC74 [doctests](https://docs.python.org/3/library/doctest.html)\uB97C\
  \ \uC2DC\uC5F0\uD558\uB294 \uC88B\uC740 \uBC29\uBC95\uC785\uB2C8\uB2E4."
lastmod: '2024-04-05T21:53:56.438829-06:00'
model: gpt-4-0125-preview
summary: "\uC774 \uC791\uC5C5\uC744 \uC790\uC8FC \uC218\uD589\uD558\uAE30 \uB54C\uBB38\
  \uC5D0 \uAC04\uB2E8\uD55C `delete()` \uD568\uC218\uB85C \uB9AC\uD329\uD1A0\uB9C1\
  \uD588\uC2B5\uB2C8\uB2E4."
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

### 나만의 커스텀 함수

이 작업을 자주 수행하기 때문에 간단한 `delete()` 함수로 리팩토링했습니다. 이건 [doctests](https://docs.python.org/3/library/doctest.html)를 시연하는 좋은 방법입니다:

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
텍스트에서 패턴과 일치하는 문자를 삭제하는 관행은 컴퓨터 과학에서 오래 전부터 있다. `sed` 및 `grep`과 같은 초기 Unix 도구들로 거슬러 올라간다. Python에서는 `re` 모듈이 이 기능을 제공하며, 정규 표현식을 이용해 텍스트 처리에 있어 강력하고 다양한 도구를 제공한다.

`re` 모듈의 대안은 다음과 같다:
- 단순한 경우를 위한 문자열 메소드 `replace()`.
- 보다 복잡한 패턴과 더 나은 유니코드 지원을 위한 제3자 라이브러리 `regex`.

내부적으로 `re.sub()`을 사용할 때, Python 해석기는 패턴을 일련의 바이트코드로 컴파일하며, 입력 텍스트에 직접 패턴 매칭을 수행하는 상태 기계에 의해 처리된다. 이 작업은 큰 문자열이나 복잡한 패턴에 대해 리소스 집약적일 수 있으므로, 대규모 데이터 처리에 있어 성능 고려사항이 중요하다.

## 참고자료
- [Python `re` 모듈 문서](https://docs.python.org/3/library/re.html): Python에서 정규 표현식을 위한 공식 문서.
- [Regular-Expressions.info](https://www.regular-expressions.info/): 정규 표현식에 대한 포괄적인 안내서.
- [Real Python에서의 regex 튜토리얼](https://realpython.com/regex-python/): Python에서의 정규 표현식의 실제 적용 사례.
