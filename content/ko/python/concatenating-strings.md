---
title:    "Python: 문자열 연결하기"
keywords: ["Python"]
---

{{< edit_this_page >}}

## 왜

문자열을 연결하는 것의 가장 좋은 이유는 여러 문자열을 하나의 문자열로 결합하여 보다 간결하고 읽기 쉬운 코드를 만들 수 있다는 것입니다. 또한 이미 존재하는 문자열을 수정하지 않고 새로운 문자열을 생성할 수 있어서 불변성(Immutability)을 유지할 수 있습니다.

## 사용 방법

```python
s1 = "안녕하세요, "
s2 = "저는 "
s3 = "파이썬 프로그래머입니다."
message = s1 + s2 + s3

print(message)
```
결과:
```
안녕하세요, 저는 파이썬 프로그래머입니다.
```

문자열을 연결하기 위해서는 "+" 연산자를 사용하면 됩니다. 이때, 모든 변수가 문자열이어야 하며, 다른 데이터 타입일 경우 에러가 발생합니다. 또한, 문자열 뿐만 아니라 변수, 숫자, 함수 등 다양한 값들을 연결할 수 있습니다.

## 깊이 파헤치기

파이썬에서 문자열은 불변성 데이터 타입으로 존재합니다. 따라서, 두 문자열을 연결하면 새로운 메모리 공간에 새로운 문자열을 생성하게 됩니다. 이러한 방식은 메모리를 효율적으로 사용하는데 도움이 됩니다. 또한, 이러한 불변성 특성 때문에 문자열을 수정할 수 없기 때문에 의도치 않은 코드 수정을 방지할 수 있습니다.

## 더 많은 정보

- [문자열 관련 파이썬 공식 문서](https://docs.python.org/3/library/stdtypes.html#text-sequence-type-str)
- [파이썬 문자열 연산자](https://www.w3schools.com/python/python_operators.asp)
- [파이썬 문자열 표현방식](https://realpython.com/python-string-formatting/)
- [파이썬 문자열 처리 관련 라이브러리](https://realpython.com/python-regex/)
- [파이썬 문자열과 리스트의 차이점](https://www.geeksforgeeks.org/python-difference-between-list-and-string/)
- [유튜브: 파이썬 문자열 연결하기](https://www.youtube.com/watch?v=DorjPR8Z0E4)

## 참고

이 블로그 포스트에서는 파이썬 3 버전을 기준으로 작성되었습니다.