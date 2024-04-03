---
changelog:
- 2024-01-28, dogweather, reviewed
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 21:24:26.117921-07:00
description: "\uBC29\uBC95: Python 3.6 \uC774\uC0C1\uC5D0\uC11C\uB294 f-strings\uB97C\
  \ \uC0AC\uC6A9\uD558\uC5EC \uBB38\uC790\uC5F4\uC5D0 \uBCF4\uAC04\uC744 \uC801\uC6A9\
  \uD560 \uC218 \uC788\uC2B5\uB2C8\uB2E4. \uBC29\uBC95\uC740 \uB2E4\uC74C\uACFC \uAC19\
  \uC2B5\uB2C8\uB2E4."
lastmod: '2024-03-13T22:44:54.577034-06:00'
model: gpt-4-0125-preview
summary: "Python 3.6 \uC774\uC0C1\uC5D0\uC11C\uB294 f-strings\uB97C \uC0AC\uC6A9\uD558\
  \uC5EC \uBB38\uC790\uC5F4\uC5D0 \uBCF4\uAC04\uC744 \uC801\uC6A9\uD560 \uC218 \uC788\
  \uC2B5\uB2C8\uB2E4."
title: "\uBB38\uC790\uC5F4 \uBCF4\uAC04\uD558\uAE30"
weight: 8
---

## 방법:
Python 3.6 이상에서는 f-strings를 사용하여 문자열에 보간을 적용할 수 있습니다. 방법은 다음과 같습니다:

```Python
name = 'Alice'
age = 30
greeting = f"안녕, {name}. 당신은 {age}세입니다."

print(greeting)
```

출력:
```
안녕, Alice. 당신은 30세입니다.
```

중괄호 안에서 표현식을 사용할 수도 있습니다:

```Python
a = 5
b = 10
info = f"5와 10을 더하면 {a + b}이고, {2 * (a + b)}이 아닙니다."

print(info)
```

출력:
```
5와 10을 더하면 15이고, 30이 아닙니다.
```

## 심화
Python 3.6 이전에는 `.format()`을 사용하여 문자열에 보간을 적용하는 것이 방식이었습니다:

```Python
name = 'Bob'
age = 25
greeting = "안녕, {}. 당신은 {}세입니다.".format(name, age)

print(greeting)
```

올드스쿨 Python(버전 < 2.6)은 보간에 `%` 연산자를 사용했으며, 이는 다수의 변수를 사용할 때 직관적이지 않고 복잡해질 수 있습니다:

```Python
name = 'Carol'
age = 35
greeting = "안녕, %s. 당신은 %d세입니다." % (name, age)

print(greeting)
```

더 깔끔한 문법 외에도, f-strings는 런타임에 평가되고 효율적인 문자열 형식 작업으로 직접 변환되기 때문에 더 빠릅니다. `.format()` 메소드와 `%` 연산자는 더 많은 단계를 거치며 더 느립니다.

## 참조
- [PEP 498 – 리터럴 문자열 보간](https://www.python.org/dev/peps/pep-0498/) 공식 문서에서 f-strings에 대해 확인하세요.
- [Python f-strings](https://realpython.com/python-f-strings/) by Real Python에서 f-strings 사용법에 대한 튜토리얼을 확인하세요.
- [The .format() Method](https://docs.python.org/3/library/stdtypes.html#str.format) Python 문서에서 오래된 `.format()` 문자열 포매팅 방법을 이해하세요.
