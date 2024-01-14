---
title:    "Python: 문자열의 길이 찾기"
keywords: ["Python"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/ko/python/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## 왜

문자열의 길이를 찾는 일은 많은 프로그래밍 언어에서 필수적인 과정입니다. 이 글에서는 파이썬으로 문자열의 길이를 찾는 방법을 살펴보겠습니다.

## 어떻게

문자열의 길이를 찾는 방법은 간단합니다. 파이썬에서는 `len()` 함수를 사용하면 됩니다. 아래는 예시 코드와 출력 결과입니다.

```Python
# 문자열의 길이 찾기
string = "안녕하세요"
print(len(string))

# 출력 결과: 5
```

위의 예시 코드에서, `string` 변수에 저장된 문자열의 길이를 `len()` 함수를 통해 찾아 출력하는 모습을 볼 수 있습니다.

## 딥 다이브

문자열의 길이를 찾는 방법에 대해 더 깊이 들어가보겠습니다. 파이썬에서 길이를 찾는 방식은 인덱싱과 관련이 있습니다. 파이썬에서 문자열은 문자들의 배열로 이루어져 있기 때문에, 문자열의 길이는 문자들의 개수와 같기 때문입니다.

또한, 공백 문자 또한 문자열의 길이에 포함됩니다. "안녕하세요"라는 문자열에서 공백을 포함해 길이를 찾으면 5가 아닌 6이 출력됩니다. 이 부분을 유의하여 문자열의 길이를 찾으시면 됩니다.

## 참고

- [파이썬 문서 - len() 함수](https://docs.python.org/ko/3.8/library/functions.html#len)
- [점프 투 파이썬 - 문자열 공부하기](https://wikidocs.net/13)
- [W3Schools - Python Strings](https://www.w3schools.com/python/python_strings.asp)

## 참고하기

- [파이썬 문서 - len() 함수](https://docs.python.org/ko/3.8/library/functions.html#len)
- [점프 투 파이썬 - 문자열 공부하기](https://wikidocs.net/13)
- [W3Schools - Python Strings](https://www.w3schools.com/python/python_strings.asp)