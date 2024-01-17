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

## 무엇이며 왜?
문자열을 연결하는 것이 무엇인지, 그리고 프로그래머들이 왜 이를 수행하는지에 대해 설명해보겠습니다. 문자열 연결은 두 개 이상의 문자열을 이어 붙여 하나의 문자열로 만드는 것을 말합니다. 이를 통해 프로그래머들은 다른 문자열 연산을 수행하기 전에 데이터를 더 쉽게 조작하고 구조화할 수 있습니다. 또한 다른 서비스와 통합하거나 데이터를 출력할 때 사람이 읽을 수 있는 형태로 변환하기 위해 문자열을 연결하는 경우도 많이 있습니다.

## 어떻게 하나요?
아래 코드 블록에 있는 예제와 출력을 참고하여 문자열을 연결하는 방법을 알아보세요.
```Python
# 기본적인 문자열 연결 방법
str1 = "Hello"
str2 = "world"
str3 = str1 + str2
print(str3) # 출력: Helloworld

# 숫자와 문자열을 함께 연결하는 방법
num = 123
str4 = "The number is " + str(num)
print(str4) # 출력: The number is 123

# 문자열을 여러 번 반복하여 연결하는 방법
str5 = "ha" * 3
print(str5) # 출력: hahaha
```

## 깊이 파헤치기
문자열 연결은 프로그래밍의 초창기부터 사용된 기술입니다. 초기에는 하드웨어와 소프트웨어의 성능이 제한되어 있었기 때문에, 문자열 연결을 사용하여 데이터를 쉽게 처리하고 저장할 수 있었습니다. 또한 문자열을 조작하는 다른 방법으로는 용이하지 않은 경우에도 문자열 연결은 유용한 대안이 될 수 있습니다. 그러나 프로그램의 성능을 최적화하려는 경우에는 직접 문자열을 조작하는 방법이 더 효율적일 수 있으니 주의하세요.

## 관련 자료
- [Python 공식 문서](https://docs.python.org/3/library/string.html): 문자열 연결에 대한 자세한 설명과 내장 함수에 대한 정보를 포함하고 있습니다.
- [Real Python 블로그 글](https://realpython.com/python-string-split-concatenate-join/): 문자열을 나누고 연결하는 다양한 방법을 소개하고 있습니다.
- [TECH ADMISSION 블로그 글](https://techadmission.net/2019/06/19/5-python-strings-operations/): 문자열을 처리하는 다양한 기술에 대해 설명하고 있습니다.