---
title:                "정규 표현식 활용하기"
date:                  2024-01-19
html_title:           "Arduino: 정규 표현식 활용하기"
simple_title:         "정규 표현식 활용하기"
programming_language: "Python"
category:             "Python"
tag:                  "Strings"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/python/using-regular-expressions.md"
---

{{< edit_this_page >}}

## What & Why? (무엇이며, 왜?)
정규 표현식은 문자열을 처리할 때 특정 패턴을 찾거나 대체할 때 사용된다. 프로그래머들은 복잡한 문자열 작업을 간편하고 효율적으로 처리하기 위해 정규 표현식을 사용한다.

## How to: (실제 사용법)
```Python
# 필요한 모듈 임포트
import re

# 문자열에서 숫자 찾기
example_string = "오늘은 2023년 3월 15일 입니다."
numbers = re.findall(r'\d+', example_string)
print(numbers)  # ['2023', '3', '15']

# 문자열 내 전화번호 형식 검증
phone_pattern = re.compile(r'^\+?82-?\d{2,3}-?\d{3,4}-?\d{4}$')
sample_phone = '+82-10-1234-5678'
result = phone_pattern.match(sample_phone)
print(result is not None)  # True

# 이메일 형식 교체하기
email_text = "이메일 주소는 example@google.com이었습니다."
new_email_text = re.sub(r'[\w\.-]+@[\w\.-]+', 'hidden-email', email_text)
print(new_email_text)  # 이메일 주소는 hidden-email이었습니다.
```

## Deep Dive (심층 분석)
정규 표현식은 1950년대 초 비롯된 수학적 이론에서 유래했다. 써드파티 라이브러리 없이 Python의 내장 `re` 모듈로 대부분의 작업을 수행할 수 있다. 그러나 대규모 데이터 처리 시에는 정규 표현식 대신 다른 문자열 처리 방법이 더 효율적일 수 있다. 구현 시, 장황한 패턴보다는 간결하고 명확한 패턴을 사용하는 것이 보기 좋고 이해하기 쉽다.

## See Also (더보기)
- Python 공식 문서: https://docs.python.org/ko/3/library/re.html
- 정규 표현식 튜토리얼: https://www.regular-expressions.info/
- 온라인 정규 표현식 테스터:
  - https://regexr.com/
  - https://regex101.com/
