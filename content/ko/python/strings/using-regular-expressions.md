---
title:                "정규 표현식 사용하기"
aliases:
- /ko/python/using-regular-expressions/
date:                  2024-02-03T19:18:09.020492-07:00
model:                 gpt-4-0125-preview
simple_title:         "정규 표현식 사용하기"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/python/using-regular-expressions.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## 무엇 & 왜?
정규 표현식(regex)은 문자열에서 문자 조합을 일치시키는 데 사용되는 패턴입니다. 프로그래머들은 정의된 패턴을 기반으로 텍스트 검색, 편집, 또는 조작하는 데 이를 활용하여 데이터 검증, 파싱, 변환과 같은 작업에 필수적입니다.

## 사용 방법:
Python에서 regex를 사용하려면 텍스트를 정규 표현식을 사용하여 처리하는 일련의 함수를 제공하는 `re` 모듈을 포함합니다.

### 기본 패턴 매칭
문자열에서 패턴을 검색하려면 `re.search()`를 사용하십시오. 패턴이 발견되면 매치 객체를 반환하고, 그렇지 않으면 `None`을 반환합니다.
```python
import re

text = "Learn Python programming"
match = re.search("Python", text)
if match:
    print("패턴을 찾았습니다!")
else:
    print("패턴을 찾지 못했습니다.")
```
출력:
```
패턴을 찾았습니다!
```

### 정규 표현식 컴파일하기
동일한 패턴을 반복해서 사용할 경우, 더 나은 성능을 위해 먼저 `re.compile()`로 컴파일하십시오.
```python
pattern = re.compile("Python")
match = pattern.search("Learn Python programming")
if match:
    print("컴파일된 패턴을 찾았습니다!")
```
출력:
```
컴파일된 패턴을 찾았습니다!
```

### 문자열 분리하기
정규 표현식 패턴의 각 매치에서 문자열을 분리하려면 `re.split()`을 사용하십시오.
```python
result = re.split("\s", "Python is fun")
print(result)
```
출력:
```
['Python', 'is', 'fun']
```

### 모든 매치 찾기
패턴의 모든 비중첩 발생을 찾으려면 `re.findall()`을 사용하십시오.
```python
matches = re.findall("n", "Python programming")
print(matches)
```
출력:
```
['n', 'n']
```

### 텍스트 대체하기
새 문자열로 패턴의 발생을 대체하려면 `re.sub()`을 사용하십시오.
```python
replaced_text = re.sub("fun", "awesome", "Python is fun")
print(replaced_text)
```
출력:
```
Python is awesome
```

### 타사 라이브러리
Python의 내장된 `re` 모듈은 강력하지만, `regex`와 같은 타사 라이브러리는 더 많은 기능과 향상된 성능을 제공합니다. `regex`를 사용하려면 pip를 통해 설치하십시오(`pip install regex`) 및 코드에서 임포트하십시오.

```python
import regex

text = "Learning Python 3.8"
match = regex.search(r"Python\s(\d+\.\d+)", text)
if match:
    print(f"찾은 버전: {match.group(1)}")
```
출력:
```
찾은 버전: 3.8
```
