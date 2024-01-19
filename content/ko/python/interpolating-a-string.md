---
title:                "문자열 보간하기"
html_title:           "Clojure: 문자열 보간하기"
simple_title:         "문자열 보간하기"
programming_language: "Python"
category:             "Python"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/python/interpolating-a-string.md"
---

{{< edit_this_page >}}

## 무엇이며 왜 사용하는가?

문자열 보간은 변수 또는 표현식을 문자열 로직에 삽입하는 방법입니다. 프로그래머는 당면한 문제에 대한 동적 해결책을 제공하기 위해 이를 사용합니다.

## 어떻게 사용하는가:

문자열 포맷팅의 가장 간단한 방법 중 하나는 `f-string`과 `{}` 을 사용하는 것입니다. 아래의 코드 예시를 확인해보세요.

```Python 
name = 'World'
print(f'Hello, {name}!')
```

위의 코드는 'Hello, World!' 를 출력합니다.

또한, 합성 연산자인 `%`를 이용한 구식의 문자열 보간 방법이 있습니다.

```Python
name = 'World'
print('Hello, %s!' % name)
```

마찬가지로 'Hello, World!'를 출력합니다.

## 깊게 알아보기

문자열 보간은 다양한 언어에서 사용하는 중요한 기능으로, 초기 프로그래밍 언어부터 시작하여 Python, JavaScript 등의 최신 언어로 적용되었습니다. Python에서는 초기에 `%` 연산자를 사용하여 문자열 보간을 수행했었지만 Python 3.6부터는 훨씬 간결하고 강력한 `f-string` 방식이 도입되었습니다.

문자열 보간의 대안으로는 `format()` 메소드가 있습니다. 이 메소드는 `f-string`보다 덜 빠르지만, 더 복잡한 문자열 포맷팅을 요구하는 경우 유용할 수 있습니다.

```Python
name = 'World'
print('Hello, {}!'.format(name))
```

## 참고하면 좋은 문서

- Python 공식 문서: [f-string](https://docs.python.org/3/reference/lexical_analysis.html#f-strings), [format()](https://docs.python.org/3/library/stdtypes.html#str.format)
- Python 문자열 보간에 대한 상세한 설명: [Real Python](https://realpython.com/python-string-formatting/)