---
title:                "문자열 추출하기"
html_title:           "Python: 문자열 추출하기"
simple_title:         "문자열 추출하기"
programming_language: "Python"
category:             "Python"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/python/extracting-substrings.md"
---

{{< edit_this_page >}}

# 무엇이고 왜?

추출된 부분 문자열은 문자열에서 원하는 부분을 잘라내는 것을 말합니다. 프로그래머들은 이 작업을 하게 되는 이유는 원하는 데이터를 더 쉽게 처리하기 위해서 입니다.

# 방법:

제가 추천하는 방법은 문자열 슬라이싱입니다. ```Python 

sentence = "Hello, world!" 
substring = sentence[0:5] # "Hello"를 추출 
print(substring) # Hello 출력 ``` 위의 예시에서 볼 수 있듯이 문자열에서 원하는 부분을 단순하게 잘라낼 수 있습니다. 또 다른 방법으로는 ```Python find() ``` 메소드를 이용하는 것이 있습니다. 

# 딥 다이브:

이 기술은 C언어에서 배경을 가지고 있으며, 문자열 슬라이싱은 C언어에서 영향을 받았습니다. 또한, 다른 언어에서도 비슷한 기능을 수행하는 메소드가 있습니다. 문자열 슬라이싱을 사용하면 보다 간편하고 효율적으로 부분 문자열을 추출할 수 있습니다.

# 관련 자료 제공:

문자열 슬라이싱 외에도 다양한 문자열 처리에 관한 자료를 제공합니다. [파이썬 공식 문서](https://docs.python.org/3/library/stdtypes.html#common-string-operations)에서 문자열 메소드를 확인할 수 있습니다. 또한, [GeeksforGeeks](https://www.geeksforgeeks.org/how-to-extract-substring-between-two-strings-in-python/)에서는 부분 문자열 추출에 대한 다양한 방법을 소개하고 있습니다.