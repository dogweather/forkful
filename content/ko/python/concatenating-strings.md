---
title:                "문자열 연결하기"
html_title:           "Arduino: 문자열 연결하기"
simple_title:         "문자열 연결하기"
programming_language: "Python"
category:             "Python"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/python/concatenating-strings.md"
---

{{< edit_this_page >}}

## 무엇이며 왜 사용하는가?

문자열 병합은 두 개 이상의 문자열을 하나로 합치는 작업입니다. 이는 여러 개의 작은 문자열을 이용해 큰 문자열을 만들거나, 일정 패턴으로 바뀌는 문자열을 생성하는 데 주로 사용됩니다.

## 어떻게 사용하는가:

Python에서 문자열 병합을 다음과 같이 실행할 수 있습니다.

```Python
# '+' 연산자 사용
str1 = "안녕"
str2 = ", 세상!"
print(str1 + str2)

# 출력: "안녕, 세상!"
```

혹은 format 메서드를 사용할 수도 있습니다.

```Python
name = "박지민"
print("안녕, {}!".format(name))

# 출력: "안녕, 박지민!"
```

f-string 방식을 선택할 수도 있습니다.

```Python
name = "박지민"
print(f"안녕, {name}!")

# 출력: "안녕, 박지민!"
```

## 심화 학습:

문자열 병합은 프로그래밍의 기본적인 요소입니다. 처음에는 '+' 연산자를 이용한 간단한 문자열 병합만 있었지만, Python에서 시간과 공간 효율성을 높이기 위해 `.join()` 메서드와 `format()` 함수, 그리고 더 심플한 `f-string` 방식 등이 등장하게 되었습니다. 

`join()` 메서드는 여러 개의 문자열을 한 번에 병합하는 데 탁월하고, `format()`과 `f-string`은 더 복잡한 형식의 문자열을 만들 때 사용하기 좋습니다.

그러나, 사용하는 상황에 따라 적절한 방법이 다를 수 있습니다. 예를 들어, 크기가 큰 문자열을 병합할 때 '+' 연산자는 비효율적일 수 있습니다. 

## 참고 자료:

- Python 공식 문서: [String Methods](https://docs.python.org/3/library/stdtypes.html#string-methods)
- Python 공식 문서: [Formatted string literals](https://docs.python.org/3/reference/lexical_analysis.html#f-strings)
- Python 공식 문서: [Format String Syntax](https://docs.python.org/3/library/string.html#formatspec)