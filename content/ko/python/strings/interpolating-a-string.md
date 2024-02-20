---
changelog:
- 2024-01-28, dogweather, reviewed
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 21:24:26.117921-07:00
description: "\uBB38\uC790\uC5F4 \uBCF4\uAC04\uBC95\uC740 \uD45C\uD604\uC2DD\uC744\
  \ \uBB38\uC790\uC5F4 \uB9AC\uD130\uB7F4 \uC548\uC5D0 \uB0B4\uC7A5\uD558\uB294 \uBC29\
  \uBC95\uC785\uB2C8\uB2E4. \uD504\uB85C\uADF8\uB798\uBA38\uB294 \uC774\uB97C \uD1B5\
  \uD574 \uB3D9\uC801\uC73C\uB85C \uBB38\uC790\uC5F4\uC5D0 \uAC12\uC744 \uC0BD\uC785\
  \uD560 \uC218 \uC788\uC73C\uBA70, \uC774\uB294 \uAE30\uC874\uC758 \uBB38\uC790\uC5F4\
  \ \uACB0\uD569 \uBC29\uC2DD\uBCF4\uB2E4 \uCF54\uB4DC\uAC00 \uB354 \uC77D\uAE30 \uC27D\
  \uACE0 \uAE54\uB054\uD574\uC9D1\uB2C8\uB2E4."
lastmod: 2024-02-19 22:05:13.519077
model: gpt-4-0125-preview
summary: "\uBB38\uC790\uC5F4 \uBCF4\uAC04\uBC95\uC740 \uD45C\uD604\uC2DD\uC744 \uBB38\
  \uC790\uC5F4 \uB9AC\uD130\uB7F4 \uC548\uC5D0 \uB0B4\uC7A5\uD558\uB294 \uBC29\uBC95\
  \uC785\uB2C8\uB2E4. \uD504\uB85C\uADF8\uB798\uBA38\uB294 \uC774\uB97C \uD1B5\uD574\
  \ \uB3D9\uC801\uC73C\uB85C \uBB38\uC790\uC5F4\uC5D0 \uAC12\uC744 \uC0BD\uC785\uD560\
  \ \uC218 \uC788\uC73C\uBA70, \uC774\uB294 \uAE30\uC874\uC758 \uBB38\uC790\uC5F4\
  \ \uACB0\uD569 \uBC29\uC2DD\uBCF4\uB2E4 \uCF54\uB4DC\uAC00 \uB354 \uC77D\uAE30 \uC27D\
  \uACE0 \uAE54\uB054\uD574\uC9D1\uB2C8\uB2E4."
title: "\uBB38\uC790\uC5F4 \uBCF4\uAC04\uD558\uAE30"
---

{{< edit_this_page >}}

## 무엇 & 왜?
문자열 보간법은 표현식을 문자열 리터럴 안에 내장하는 방법입니다. 프로그래머는 이를 통해 동적으로 문자열에 값을 삽입할 수 있으며, 이는 기존의 문자열 결합 방식보다 코드가 더 읽기 쉽고 깔끔해집니다.

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
