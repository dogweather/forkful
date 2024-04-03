---
date: 2024-01-20 17:39:29.629251-07:00
description: "How to: Python\uC5D0\uC11C \uBB38\uC790\uC5F4\uC744 \uC18C\uBB38\uC790\
  \uB85C \uBC14\uAFB8\uB294 \uBC29\uBC95\uC5D0\uB294 \uC5EC\uB7EC \uAC00\uC9C0\uAC00\
  \ \uC788\uC2B5\uB2C8\uB2E4\uB9CC, \uAC00\uC7A5 \uAC04\uB2E8\uD558\uACE0 \uBCF4\uD3B8\
  \uC801\uC778 \uBC29\uBC95\uC740 `lower()` \uBA54\uC18C\uB4DC\uB97C \uC0AC\uC6A9\uD558\
  \uB294 \uAC83\uC785\uB2C8\uB2E4. \uC544\uB798 \uC608\uC81C\uB97C \uBCF4\uC138\uC694\
  ."
isCJKLanguage: true
lastmod: '2024-03-13T22:44:54.578740-06:00'
model: gpt-4-1106-preview
summary: "Python\uC5D0\uC11C \uBB38\uC790\uC5F4\uC744 \uC18C\uBB38\uC790\uB85C \uBC14\
  \uAFB8\uB294 \uBC29\uBC95\uC5D0\uB294 \uC5EC\uB7EC \uAC00\uC9C0\uAC00 \uC788\uC2B5\
  \uB2C8\uB2E4\uB9CC, \uAC00\uC7A5 \uAC04\uB2E8\uD558\uACE0 \uBCF4\uD3B8\uC801\uC778\
  \ \uBC29\uBC95\uC740 `lower()` \uBA54\uC18C\uB4DC\uB97C \uC0AC\uC6A9\uD558\uB294\
  \ \uAC83\uC785\uB2C8\uB2E4."
title: "\uBB38\uC790\uC5F4\uC744 \uC18C\uBB38\uC790\uB85C \uBCC0\uD658\uD558\uAE30"
weight: 4
---

## How to:
Python에서 문자열을 소문자로 바꾸는 방법에는 여러 가지가 있습니다만, 가장 간단하고 보편적인 방법은 `lower()` 메소드를 사용하는 것입니다. 아래 예제를 보세요.

```Python
original_string = "Hello, World!"
lowercase_string = original_string.lower()
print(lowercase_string)
```

출력:
```
hello, world!
```

단순하죠. 이 방법은 모든 표준 알파벳 문자에 적용됩니다.

## Deep Dive
문자열을 소문자로 변환하는 것은 프로그래밍의 초기부터 필요했던 기능입니다. 이것은 사용자가 입력한 데이터의 일관성을 유지하고, 대소문자가 다른 경우에도 문자열을 올바르게 비교할 수 있게 해줍니다.

`lower()` 이외의 방법으로는 문자열을 순회하며 각 문자에 대한 소문자 변환을 수동으로 수행하는 것입니다만, 이는 비효율적이고 오류가 발생하기 쉬우므로 추천되지 않습니다. Python의 `lower()` 메소드는 유니코드 문자열에 대한 전체 대소문자 매핑을 이미 알고 있어 모든 언어에 적용 가능하고 빠릅니다.

## See Also
- Python 공식 문서에서 `lower()` 메소드에 대한 자세한 정보를 확인할 수 있습니다: [Python's str.lower()](https://docs.python.org/3/library/stdtypes.html#str.lower)
- 유니코드와 문자열 인코딩에 대해 더 배우고 싶다면 [Unicode HOWTO](https://docs.python.org/3/howto/unicode.html)를 참고하세요.
- 문자열 조작에 대한 다양한 방법을 알아보려면 Python의 [string methods documentation](https://docs.python.org/3/library/stdtypes.html#string-methods)을 읽어보세요.
