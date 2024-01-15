---
title:                "문자열 연결하기"
html_title:           "Python: 문자열 연결하기"
simple_title:         "문자열 연결하기"
programming_language: "Python"
category:             "Python"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/python/concatenating-strings.md"
---

{{< edit_this_page >}}

## 왜?

문자열 연결은 Python에서 매우 자주 사용되는 작업이며, 문자열을 조합하여 더 큰 문장이나 데이터를 만들 수 있기 때문에 매우 유용합니다.

## 어떻게?

```Python
# concatenate strings using the '+' operator
first_name = "John"
last_name = "Smith"
full_name = first_name + " " + last_name
print(full_name)
# output: John Smith

# concatenating with string formatting
age = 32
print("I am {} years old.".format(age))
# output: I am 32 years old.
```

## 깊게 들어가보자!

문자열 연결에는 여러 가지 방법이 있지만, 가장 일반적인 방법은 '+' 연산자를 사용하는 것입니다. 이를 이용하여 변수나 상수를 조합하여 새로운 문자열을 만들 수 있습니다. 또한, 문자열 포맷팅을 사용하면 더 유연하고 간결한 코드를 작성할 수 있습니다. 이를 통해 변수 값을 문자열에 삽입하거나 형식화할 수 있습니다. 또한 문자열의 특정 부분을 추출하거나 수정하기 위해서는 인덱싱과 슬라이싱을 사용할 수 있습니다. 다양한 방법을 사용하여 문자열을 연결하여 보다 복잡한 작업을 수행할 수 있습니다.

## 또 다른 참고자료

- [Python에서 문자열 연결하기](https://www.geeksforgeeks.org/python-concatenation-of-two-strings/)
- [문자열 포맷팅과 관련된 공식 문서](https://docs.python.org/3/library/string.html#format-string-syntax)
- [인덱싱과 슬라이싱에 대한 자세한 설명](https://www.w3schools.com/python/python_strings.asp)

## 레퍼런스

[마크다운(Markdown) 이해하기](https://guides.github.com/features/mastering-markdown/)