---
title:                "문자열 보간"
html_title:           "Python: 문자열 보간"
simple_title:         "문자열 보간"
programming_language: "Python"
category:             "Python"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/python/interpolating-a-string.md"
---

{{< edit_this_page >}}

## 무엇 & 왜?
문자열을 보간하는 것은 기호 또는 변수를 문자열 안에 삽입하는 것을 말합니다. 이를 통해 프로그래머는 동적인 문자열을 만들고 더 효율적인 코드를 작성할 수 있습니다.

## 방법:
```python 
# 'Hello, {name}! How are you doing today?'
# 'Hello, John! How are you doing today?'

name = 'John'
print(f'Hello, {name}! How are you doing today?')
```

```python 
# 'Your total for today is ${total}. Have a nice day!'
# 'Your total for today is $25.50. Have a nice day!'

total = 25.50
print(f'Your total for today is ${total}. Have a nice day!')
```

## 깊이 파헤치기:
1. 역사적 맥락: 문자열 보간은 C언어에서부터 사용되어왔으며 최근에는 다른 프로그래밍 언어에서도 지원하고 있습니다.
2. 대안: 문자열 보간을 대체할 수 있는 다른 방법으로는 문자열 연결, % 연산자 등이 있지만 보간은 가독성과 편의성 면에서 더 나은 선택입니다.
3. 구현 세부사항: 파이썬 3.6 버전부터는 f-string을 사용하여 보간 기능을 제공하고 있습니다.

## 참고자료:
- [PEP 498: Formatted string literals](https://www.python.org/dev/peps/pep-0498/)
- [Python `str.format()` documentation](https://docs.python.org/3/library/stdtypes.html#str.format)
- [String Interpolation in Python](https://realpython.com/python-string-formatting/#1-old-style-string-formatting-operator)