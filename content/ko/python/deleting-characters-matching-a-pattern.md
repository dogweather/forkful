---
title:                "Python: 패턴과 일치하는 문자 삭제하기"
simple_title:         "패턴과 일치하는 문자 삭제하기"
programming_language: "Python"
category:             "Python"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/python/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## 왜

문자열에서 특정 패턴과 일치하는 문자를 삭제하는 것은 데이터 정리나 문자열 처리에 유용한 작업입니다.

## 방법

```python
# 문자열 변수 설정
string = "Hello World123"

# 문자열에서 숫자만 삭제하기
num = ""
for char in string:
    if char.isalpha():
        num += char
print(num)

# Output: Hello World

# 문자열에서 소문자만 삭제하기
lower = ""
for char in string:
    if not char.islower():
        lower += char
print(lower)

# Output: HW123
```

## 깊이 파고들기

일반적인 문자열 처리 기술 중 하나인 정규표현식을 사용하여 문자열에서 패턴과 일치하는 문자를 삭제할 수 있습니다. 정규표현식은 각 문자의 의미를 나타내는 패턴을 정의하고, 문자열에서 해당 패턴과 일치하는 부분을 추출 또는 삭제하는 데 사용됩니다. 예를 들어, 소문자를 삭제하기 위해 정규표현식 `[a-z]`를 사용할 수 있습니다.

## 참고 자료

[정규표현식에 대한 자세한 설명](https://wikidocs.net/4308)  
[문자열 처리 관련 파이썬 라이브러리](https://docs.python.org/3/library/string.html)  
[파이썬 정규표현식 라이브러리](https://docs.python.org/3/library/re.html)