---
title:    "Python: 문자열 대문자화"
keywords: ["Python"]
---

{{< edit_this_page >}}

## 왜

문자열의 첫 글자를 대문자로 바꾸는 것은 프로그래밍에서 매우 일반적인 작업입니다. 이 작업을 수행하는 이유는 다양합니다. 대문자로 시작하는 문자열은 주로 제목이나 문장의 첫 글자를 대문자로 표시하기 위해 사용됩니다. 또는 입력된 문자열이 대소문자를 구분하는 작업을 수행할 때 유용할 수 있습니다.

## 어떻게

문자열을 대문자로 바꾸는 것은 Python에서 매우 간단합니다. 우선, 문자열을 변수에 할당한 다음 `capitalize()` 메서드를 사용하면 됩니다. 아래의 예제 코드를 참고하세요.

```Python
string = "hello world"
print(string.capitalize())
```

위 코드의 결과는 "Hello world"가 됩니다.

## 깊게 파고들기

문자열의 첫 글자를 대문자로 바꾸는 방법에 대해 좀 더 깊게 알아보겠습니다. `capitalize()` 메서드는 해당 문자열의 첫 글자를 대문자로 변환하는 것 외에도, 그 다음 글자부터는 모두 소문자로 변환하고 나머지 글자는 그대로 유지합니다. 예를 들어, "heLlO WoRLd"라는 문자열에 `capitalize()` 메서드를 적용하면 "Hello world"가 아닌 "Hello World"가 됩니다. 이러한 특징을 이해하고 적절하게 활용하는 것이 중요합니다.

## 가장 참고할만한

- [Python 문자열](https://docs.python.org/3/library/stdtypes.html#text-sequence-type-str)
- [문자열 메서드](https://www.w3schools.com/python/python_ref_string.asp)
- [온라인 Python 코스](https://www.codecademy.com/learn/learn-python)