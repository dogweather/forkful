---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:18:09.020492-07:00
description: "\uC815\uADDC \uD45C\uD604\uC2DD(regex)\uC740 \uBB38\uC790\uC5F4\uC5D0\
  \uC11C \uBB38\uC790 \uC870\uD569\uC744 \uC77C\uCE58\uC2DC\uD0A4\uB294 \uB370 \uC0AC\
  \uC6A9\uB418\uB294 \uD328\uD134\uC785\uB2C8\uB2E4. \uD504\uB85C\uADF8\uB798\uBA38\
  \uB4E4\uC740 \uC815\uC758\uB41C \uD328\uD134\uC744 \uAE30\uBC18\uC73C\uB85C \uD14D\
  \uC2A4\uD2B8 \uAC80\uC0C9, \uD3B8\uC9D1, \uB610\uB294 \uC870\uC791\uD558\uB294 \uB370\
  \ \uC774\uB97C \uD65C\uC6A9\uD558\uC5EC \uB370\uC774\uD130 \uAC80\uC99D, \uD30C\uC2F1\
  , \uBCC0\uD658\uACFC \uAC19\uC740 \uC791\uC5C5\uC5D0 \uD544\uC218\uC801\uC785\uB2C8\
  \uB2E4."
lastmod: '2024-02-25T18:49:51.620676-07:00'
model: gpt-4-0125-preview
summary: "\uC815\uADDC \uD45C\uD604\uC2DD(regex)\uC740 \uBB38\uC790\uC5F4\uC5D0\uC11C\
  \ \uBB38\uC790 \uC870\uD569\uC744 \uC77C\uCE58\uC2DC\uD0A4\uB294 \uB370 \uC0AC\uC6A9\
  \uB418\uB294 \uD328\uD134\uC785\uB2C8\uB2E4. \uD504\uB85C\uADF8\uB798\uBA38\uB4E4\
  \uC740 \uC815\uC758\uB41C \uD328\uD134\uC744 \uAE30\uBC18\uC73C\uB85C \uD14D\uC2A4\
  \uD2B8 \uAC80\uC0C9, \uD3B8\uC9D1, \uB610\uB294 \uC870\uC791\uD558\uB294 \uB370\
  \ \uC774\uB97C \uD65C\uC6A9\uD558\uC5EC \uB370\uC774\uD130 \uAC80\uC99D, \uD30C\uC2F1\
  , \uBCC0\uD658\uACFC \uAC19\uC740 \uC791\uC5C5\uC5D0 \uD544\uC218\uC801\uC785\uB2C8\
  \uB2E4."
title: "\uC815\uADDC \uD45C\uD604\uC2DD \uC0AC\uC6A9\uD558\uAE30"
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
