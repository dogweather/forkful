---
title:                "Python: 정규 표현식 사용하기"
simple_title:         "정규 표현식 사용하기"
programming_language: "Python"
category:             "Python"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/python/using-regular-expressions.md"
---

{{< edit_this_page >}}

## 왜

정규식을 사용하는 것은 문자열로 작업하는 것을 더 쉽게 만들어줍니다. 예를 들어, 특정 패턴이나 형식으로 된 문자열만 찾아야 할 때 정규식을 사용하면 매우 유용합니다.

## 방법

정규식을 사용하려면 먼저 `re` 모듈을 불러와야 합니다. 문자열 변수를 정의하고, 이 변수에서 원하는 패턴을 찾아내기 위해 `re.search()` 함수를 사용합니다. 아래는 입력된 문자열에서 이메일 형식의 패턴을 찾는 예제입니다.

```python
import re

# 문자열 변수 정의
string = "My email is example@example.com"

# 이메일 형식의 패턴 찾기
result = re.search(r'\w+@\w+\.\w+', string)

# 결과 출력
print(result.group())
```

위의 예제에서 `re.search()` 함수는 입력된 문자열에서 이메일 형식의 패턴을 찾아내고 `result` 변수에 저장합니다. 그리고 `result.group()` 함수를 사용해서 찾은 패턴을 출력합니다. 결과는 다음과 같이 나옵니다.

```
example@example.com
```

## 깊게 파고들기

정규식 패턴을 사용할 때는 조금 더 복잡한 패턴을 찾을 수도 있습니다. 예를 들어, 이메일 주소의 도메인 이름만을 추출하고 싶을 수 있습니다. 이럴 때는 `re.search()` 함수를 조금 수정하면 됩니다. 아래는 이메일 주소에서 도메인 이름만을 추출하는 예제입니다.

```python
import re

# 문자열 변수 정의
string = "My email is example@example.com"

# 이메일 주소에서 도메인 이름만 추출
result = re.search(r'@(\w+\.\w+)', string)

# 결과 출력
print(result.group(1))
```

위의 예제에서 `@` 기호를 기준으로 `\.w+` 패턴을 검색하고, 그룹 매칭을 사용해 이 패턴을 `result.group(1)`에서 출력합니다. 그리고 결과는 다음과 같이 나옵니다.

```
example.com
```

## 관련 정보

[파이썬 정규식 문서](https://docs.python.org/3/library/re.html)

[정규식 예제 모음](https://regexr.com/)

[정규식 연습 사이트](https://regex101.com/)