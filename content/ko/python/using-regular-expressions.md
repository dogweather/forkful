---
title:                "정규 표현식 활용하기"
html_title:           "Python: 정규 표현식 활용하기"
simple_title:         "정규 표현식 활용하기"
programming_language: "Python"
category:             "Python"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/python/using-regular-expressions.md"
---

{{< edit_this_page >}}

## 왜?
정규식은 텍스트에서 패턴을 찾거나 대체, 분리하는 등 다양한 문자열 작업을 더 쉽고 효율적으로 수행할 수 있게 해줍니다. 따라서 개발자들은 코드를 더 짧고 간결하게 작성하며, 작업 시간을 단축할 수 있습니다.

## 사용법
정규식을 사용하기 위해서는 `re` 모듈을 import해야 합니다. 다음은 간단한 이메일 주소를 찾는 예제 코드입니다.
```python
import re

# 정규식 패턴 생성
pattern = r'[\w\.-]+@[\w\.-]+\.[\w]+'

# 텍스트에서 패턴 찾기
text = "저의 이메일 주소는 abc123@gmail.com입니다."
match = re.search(pattern, text)

# 매치 결과 출력
print(match.group())

# Output: abc123@gmail,com
```

위 코드에서 사용된 `r`은 Raw String을 의미하며, 정규식 패턴을 더 간결하게 작성할 수 있도록 해줍니다. `re.search()`는 텍스트에서 첫 번째로 매치되는 패턴을 찾아내며, `group()` 함수를 사용하여 매치된 결과를 출력합니다.

## 깊이있게 알아보기
정규식에서 사용되는 메타 문자(`[]` `+` `.` 등)에 대해 자세히 알아보고, 각각의 의미와 사용법을 숙지하는 것이 중요합니다. 또한 패턴 매칭에서 중복되는 부분을 더 쉽게 처리하기 위한 그룹화, 더 복잡한 패턴을 작성하기 위한 특수 시퀀스 등에 대해 공부하는 것도 중요합니다.

## 더 알아보기
- [Python 정규식 공식 문서(한국어)](https://docs.python.org/ko/3/library/re.html)
- [번역임의(https://python-regexp.readthedocs.io/en/latest/tutorial.html)
- [정규식 테스트 사이트(https://regex101.com/)