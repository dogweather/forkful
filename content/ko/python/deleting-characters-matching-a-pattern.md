---
title:                "패턴과 일치하는 문자 삭제"
html_title:           "Python: 패턴과 일치하는 문자 삭제"
simple_title:         "패턴과 일치하는 문자 삭제"
programming_language: "Python"
category:             "Python"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/python/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## 뭐 & 왜?
문자 패턴과 일치하는 문자를 삭제하는 것은 프로그래머들이 코드에서 특정 문자를 제거하는 방법입니다. 이를 통해 코드가 더욱 간결하고 읽기 쉬워지며 잘못된 문자를 수정하는 데 도움이 됩니다.

## 방법:
파이썬에서 문자 패턴에 일치하는 문자를 삭제하는 방법은 간단합니다. 문자열 메소드 `replace()`를 사용하면 됩니다. 예를 들어, 아래 코드는 `Hello World` 문자열에서 모든 공백을 제거합니다.

```Python
string = "Hello World"
new_string = string.replace(" ", "")
print(new_string) # 출력: HelloWorld
```

또 다른 예로, 아래 코드는 `123 abc 456` 문자열에서 숫자만 남깁니다.

```Python
string = "123 abc 456"
new_string = ''.join(i for i in string if i.isdigit())
print(new_string) # 출력: 123456
```

## 깊게 파보기:
문자를 삭제하는 방법은 문자열 처리에 있어서 매우 중요합니다. 예전에는 정규식을 사용해야 했지만 파이썬에서 제공하는 문자열 메소드를 이용하면 더욱 간단하게 문자를 삭제할 수 있습니다. 또한, `strip()` 메소드를 사용하면 문자열의 양 끝에 있는 특정 문자를 제거할 수도 있습니다.

## 더 찾아보기:
- [파이썬 공식 문서: 문자열 메소드](https://docs.python.org/3/library/stdtypes.html#string-methods)
- [점프 투 파이썬: 고급 문자열 포맷팅](https://wikidocs.net/13#_12)
- [W3Schools: 파이썬 문자열 메소드](https://www.w3schools.com/python/python_ref_string.asp)