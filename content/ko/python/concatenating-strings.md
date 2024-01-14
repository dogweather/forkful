---
title:                "Python: 문자열 연결하기"
programming_language: "Python"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/python/concatenating-strings.md"
---

{{< edit_this_page >}}

## 왜
문자열을 연결하는 것에 참여하는 이유는 여러 가지가 있습니다. 파일 이름을 만들거나 웹 URL을 생성하는 등의 상황에서 유용하게 사용할 수 있습니다.

## 하는 방법
일반적으로 Python에서는 "+" 연산자를 사용하여 문자열을 연결할 수 있습니다. 또는 문자열의 format() 메서드를 사용하여 변수를 삽입할 수도 있습니다.

```Python
a = "안녕하세요,"
b = "제 이름은"
name = "John"
c = a + b + name
print(c)
# 결과: 안녕하세요, 제 이름은 John
```
format() 메서드를 사용하면 다음과 같습니다.

```Python
a = "안녕하세요, 제 이름은 {}입니다."
name = "John"
c = a.format(name)
print(c)
# 결과: 안녕하세요, 제 이름은 John입니다.
```

또 다른 방법으로는 문자열을 리스트로 만든 다음 join() 함수를 사용하는 방법도 있습니다.

```Python
a = ["안녕하세요,", "제 이름은", "John입니다."]
b = " ".join(a)
print(b)
# 결과: 안녕하세요, 제 이름은 John입니다.
```

## 깊이있는 분석
Python에서 문자열을 연결하는 내부 동작은 실제로 문자열 객체의 메모리 주소를 변경하는 것입니다. 이는 immutable한 객체인 문자열을 합칠 때 새로운 객체가 생성되는 것이 아니라 기존의 객체가 변경되는 것을 의미합니다.

또한 "+" 연산자를 사용하는 것보다 format() 메서드를 사용하는 것이 성능 면에서 더 우수합니다. 하지만 적은 양의 문자열을 연결하는 경우에는 큰 차이를 보이지 않습니다.

## 참고 자료
- [Python 문자열 연결 문서](https://docs.python.org/3/library/stdtypes.html#str)
- [Python format() 메서드 문서](https://www.w3schools.com/python/ref_string_format.asp)
- [Python join() 함수 문서](https://www.w3schools.com/python/ref_string_join.asp)