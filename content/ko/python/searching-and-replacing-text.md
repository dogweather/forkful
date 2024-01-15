---
title:                "텍스트 검색 및 교체"
html_title:           "Python: 텍스트 검색 및 교체"
simple_title:         "텍스트 검색 및 교체"
programming_language: "Python"
category:             "Python"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/python/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## 왜
텍스트를 찾고 대체하는 것을 하려면 왜 누군가가 참여할까요? 간단히 말해서, 이것은 효율적으로 많은 작업을 수행하고 코드를 업데이트하는 데 유용한 방법입니다.

## 코드 예제
```Python
# 텍스트 대체하기
text = "안녕하세요. 오늘은 기분 좋은 날입니다."
new_text = text.replace("기분 좋은", "우리집에 놀러와서")
print(new_text)

# 출력 결과: "안녕하세요. 오늘은 우리집에 놀러와서 날입니다."
```

```Python
# 정규식을 사용하여 특정 패턴의 텍스트 찾기
import re
text = "파이썬 3.9 버전이 출시되었습니다."
pattern = "(\d+\.\d+)"
match = re.search(pattern, text)
print("발견된 버전: " + match.group())

# 출력 결과: "발견된 버전: 3.9"
```

## 깊이 있게 살펴보기
- `replace()` 함수는 문자열에서 특정 문자열을 다른 문자열로 대체하는 데 사용됩니다.
- 정규식(regular expression)은 텍스트에서 특정 패턴을 찾는 데 유용하며, 파이썬의 `re` 모듈을 사용하여 구현할 수 있습니다.
- `search()` 함수는 텍스트에서 정규식 패턴과 일치하는 첫 번째 값을 찾아주는 역할을 합니다.

## 관련 링크
- [파이썬 문자열 관련 함수 공식 문서](https://docs.python.org/3/library/stdtypes.html#string-methods)
- [정규식 표현 패턴 및 사용법](https://docs.python.org/3/library/re.html)