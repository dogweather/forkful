---
title:                "문자열 대문자로 변환하기"
date:                  2024-01-19
html_title:           "Arduino: 문자열 대문자로 변환하기"
simple_title:         "문자열 대문자로 변환하기"

category:             "Python"
tag:                  "Strings"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/python/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## What & Why? (무엇이며 왜?)
문자열 대문자화는 모든 글자를 대문자로 바꾸는 것입니다. 이를 통해 텍스트가 더 눈에 띄게 하거나, 데이터를 표준화하는 데 종종 사용됩니다.

## How to: (어떻게 하나요?)
```python
# 문자열 대문자로 변환하기
text = "hello world"
capitalized_text = text.upper()

print(capitalized_text)  # HELLO WORLD
```

```python
# 리스트 안의 문자열 대문자로 변환하기
words = ["python", "programming", "tutorial"]
capitalized_words = [word.upper() for word in words]

print(capitalized_words)  # ['PYTHON', 'PROGRAMMING', 'TUTORIAL']
```

## Deep Dive (심층 탐구)
- **역사적 배경**: 과거에는 대문자만 표기할 수 있는 타자기가 있었습니다. 오늘날, 프로그래밍에 있어서 대소문자 변환 기능은 사용자 입력을 표준화하고, 다양한 컴퓨터 시스템 간 호환성을 확보하는 데 중요해졌습니다.
- **대안**: 파이썬에서 `.capitalize()` 는 첫 글자만 대문자로 변환합니다, `.upper()` 는 모든 문자를 대문자로 변환합니다. 또한 `.title()`은 모든 단어의 시작 글자를 대문자로 변환합니다.
- **구현 디테일**: `.upper()` 메소드는 유니코드 표준에 따라 문자열 속의 각 문자를 해당하는 대문자로 매핑합니다. 어떤 문자에는 대문자가 존재하지 않을 수도 있는데, 이 경우 해당 문자는 변하지 않습니다.

## See Also (추가 정보)
- 파이썬 공식 문서: https://docs.python.org/3/library/stdtypes.html#str.upper
- 유니코드 표준: http://www.unicode.org/reports/tr21/tr21-5.html
- 문자열 메서드 관련 튜토리얼: https://realpython.com/python-strings/#built-in-string-methods
