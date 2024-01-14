---
title:    "Python: 문자열 합치기"
keywords: ["Python"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/ko/python/concatenating-strings.md"
---

{{< edit_this_page >}}

## 왜

문자열을 연결하는 것에 참여하는 이유는 간단합니다. 여러 개의 문자열을 하나의 문자열로 결합하면 유용한 출력을 만들 수 있습니다. 문자열을 대/소문자로 변환하거나 여러 개의 문자열을 추출하여 새로운 문장을 만드는 등 다양한 용도로 사용할 수 있습니다.

## 하는 방법

우선, 파이썬의 문자열 연결 기능 중 하나인 `+` 연산자를 사용하여 여러 개의 문자열을 결합할 수 있습니다.

```Python
string_1 = "안녕하세요, "
string_2 = "저는 "
string_3 = "파이썬 프로그래머입니다."
result = string_1 + string_2 + string_3

print(result)
```

출력 결과는 다음과 같습니다.

```
안녕하세요, 저는 파이썬 프로그래머입니다.
```

또는 `join()` 메서드를 사용하여 리스트의 문자열을 결합할 수도 있습니다.

```Python
languages = ["HTML", "CSS", "JavaScript"]

result = ", ".join(languages)

print(result)
```

출력 결과는 다음과 같습니다.

```
HTML, CSS, JavaScript
```

## 더 들어가기

파이썬의 문자열 연결 기능은 단순히 두 개의 문자열을 결합하는 것을 넘어서 다양한 기능을 제공합니다. 예를 들어, `*` 연산자를 사용하여 문자열을 반복할 수 있고 `in` 연산자를 사용하여 문자열 속에 특정 단어가 있는지 확인할 수 있습니다.

또한 f-string을 사용하면 변수를 문자열에 포함시켜서 동적인 문장을 만들 수 있습니다. 이를 통해 보다 복잡한 출력과 코드를 만들 수 있습니다.

```
name = "Jane"
age = 25

print(f"저의 이름은 {name}이고, 나이는 {age}살입니다.")
```

출력 결과는 다음과 같습니다.

```
저의 이름은 Jane이고, 나이는 25살입니다.
```

## 참고

다른 문자열 작업에 대해 자세히 알고 싶다면 다음 링크를 참고해 보세요.

- [Python 문자열 관련 문서](https://docs.python.org/ko/3/library/stdtypes.html#string-methods)
- [파이썬 f-string 소개](https://realpython.com/python-f-strings/)
- [Join() 메서드 상세 설명](https://www.w3schools.com/python/ref_string_join.asp)

## 참고하기

- [파이썬 공식 문서](https://www.python.org/)
- [Codecademy Python 강의](https://www.codecademy.com/learn/learn-python)