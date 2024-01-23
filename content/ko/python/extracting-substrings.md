---
title:                "부분 문자열 추출"
date:                  2024-01-20T17:46:25.084044-07:00
model:                 gpt-4-1106-preview
simple_title:         "부분 문자열 추출"
programming_language: "Python"
category:             "Python"
tag:                  "Strings"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/python/extracting-substrings.md"
---

{{< edit_this_page >}}

## What & Why? (무엇과 왜?)
문자열에서 부분 문자열을 추출하는 것은 문자열의 특정 부분을 선택하는 것을 말합니다. 프로그래머들은 데이터를 파싱하거나 특정 패턴을 찾을 때 이를 자주 사용합니다.

## How to: (어떻게 하나요?)
```python
# 샘플 문자열
text = "파이썬은 재미있습니다!"

# 인덱싱과 슬라이싱을 사용하여 부분 문자열 추출하기
first_word = text[:5]           # 시작부터 5번째 인덱스 전까지
print(first_word)               # 출력: 파이썬은

last_word = text[-5:]           # 마지막에서 5번째 인덱스부터 끝까지
print(last_word)                # 출력: 있습니다!

specific_part = text[6:9]       # 6번째 인덱스부터 9번째 인덱스 전까지
print(specific_part)            # 출력: 재미있
```

## Deep Dive (심층 분석)
문자열은 Python에서 불변의 시퀀스 타입입니다. 따라서 원본 문자열은 변경되지 않고 새로운 문자열이 생성됩니다. 슬라이싱은 파이썬 초기 버전부터 제공되었으며, 매우 효율적입니다. 

파이썬에는 정규 표현식을 사용한 부분 문자열 추출도 가능합니다. `re` 모듈은 강력한 문자열 처리를 지원하고 다양한 패턴과 일치하는 복잡한 문자열을 찾을 때 사용됩니다.

부분 문자열의 잘림(sharding)이나 추출을 위한 구현 세부사항은 파이썬의 C 기반 엔진 내부에서 처리됩니다. 문자열 객체가 보관하고 있는 문자 데이터에 대하여, 파이썬의 슬라이스 표현식은 해당 데이터를 참조하는 새로운 문자열 객체를 만듭니다.

## See Also (관련 자료)
- Python's official documentation on strings: https://docs.python.org/3/tutorial/introduction.html#strings
- Python's `re` module for regular expressions: https://docs.python.org/3/library/re.html
- Python String Methods: https://docs.python.org/3/library/stdtypes.html#string-methods

Remember, these techniques can be very handy when working with text data, and Python makes it incredibly easy to work with strings. Keep slicing and dicing those strings!
