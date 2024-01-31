---
title:                "패턴에 일치하는 문자 삭제"
date:                  2024-01-20T17:42:58.775533-07:00
model:                 gpt-4-1106-preview
simple_title:         "패턴에 일치하는 문자 삭제"

tag:                  "Strings"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/python/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## What & Why? (무엇 & 왜?)
문자열에서 특정 패턴을 매칭하는 문자들을 삭제하는 것은 데이터를 정제하고 원하는 형태로 가공하기 위해 필요합니다. 프로그래머들은 잡음이 없는, 깨끗한 데이터를 분석하거나 사용자가 입력한 정보를 처리할 때 이 기술을 사용합니다.

## How to: (방법)
```Python
import re

# 주어진 문자열
text = "abc123!@#ABC"

# 정규 표현식을 사용하여 숫자와 특수문자 제거
cleaned_text = re.sub('[0-9!@#]', '', text)

print(cleaned_text)  # 출력: abcABC
```
주의: `re.sub()` 함수는 정규 표현식 패턴에 매칭되는 모든 문자를 삭제합니다.

## Deep Dive (심화 탐구)
문자 삭제는 컴퓨팅 초창기부터 문자열 처리에서 중요한 작업이었습니다. 역사적으로 문자열 데이터를 다룰 때는 메모리와 성능 제약으로 인해 최적화가 중요했습니다.

### 대안:
- `str.replace()`: 간단한 삭제에 사용할 수 있지만, 패턴이 복잡할 때는 한계가 있습니다.
- `str.translate()`: 딕셔너리를 이용해 여러 문자를 한 번에 매핑 삭제할 수 있습니다.

### 구현 세부사항:
- `re.sub()`: 정규표현식은 강력하고 유연하며 복잡한 패턴을 다룰 때 이상적입니다. 내부적으로는 오브젝트를 컴파일하여 패턴 매칭을 수행합니다.

## See Also (추가 자료)
- Python 정규 표현식 문서: https://docs.python.org/3/library/re.html
- 문자열 메서드 공식 문서: https://docs.python.org/3/library/stdtypes.html#string-methods
- 정규 표현식에 관한 추가 정보: https://www.regular-expressions.info/
