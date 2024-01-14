---
title:                "Python: 부분 문자열 추출"
simple_title:         "부분 문자열 추출"
programming_language: "Python"
category:             "Python"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/python/extracting-substrings.md"
---

{{< edit_this_page >}}

## 왜
문자열에서 부분 문자열을 추출하는 일은 프로그래밍에서 매우 일반적인 작업 중 하나입니다. 이를 통해 우리는 특정 문자열에서 원하는 정보를 추출하고 다양한 작업에 활용할 수 있습니다.

## 추출 방법
문자열에서 부분 문자열을 추출하는 방법은 간단합니다. 먼저 `[]`를 사용하여 추출하려는 부분 문자열의 시작과 끝 인덱스를 지정해야 합니다. 아래는 파이썬으로 문자열에서 부분 문자열을 추출하는 예시입니다.

```Python
example_string = "안녕하세요! 저는 파이썬을 배우고 있어요."
# "파이썬"이라는 부분 문자열 추출
substring = example_string[9:12]
# 출력: 파이썬
print(substring)
```

위 코드에서 `example_string` 변수에 문자열을 저장하고, `substring` 변수에는 `[9:12]`를 사용하여 "파이썬"이라는 부분 문자열을 추출했습니다. 문자열의 인덱스는 0부터 시작하기 때문에 `"파이썬"`이 시작하는 위치의 인덱스는 9이고, 끝나는 위치의 인덱스는 12 전인 11이 됩니다. 따라서 `[9:12]`를 사용하여 문자열의 9번째에서 11번째까지의 문자를 추출하였습니다.

이외에도 파이썬의 내장 함수인 `find()`, `split()` 등을 활용하여 부분 문자열을 추출할 수 있으니, 상황에 맞게 필요한 방법을 찾아 사용하시면 됩니다.

## 깊게 파고들기
부분 문자열을 추출하는 과정에서 우리에게 가장 중요한 것은 인덱스를 제대로 사용하는 것입니다. 인덱스의 시작은 항상 0이며, 끝 인덱스는 실제 추출하고자 하는 문자열의 마지막 인덱스보다 1 작아야 합니다. 또한 해당 문자열의 길이를 초과하는 인덱스를 지정할 경우 오류가 발생하니 주의해야 합니다.

참고로 파이썬에서는 인덱스를 음수로 지정할 수도 있습니다. `-1`은 마지막 문자를 의미하고, `-2`는 마지막에서 두번째 문자를 의미합니다. 따라서 문자열 끝부터 추출하고 싶을 때 `-n` 형태의 인덱스를 사용하시면 됩니다.

## 더 알아보기
문자열에서 부분 문자열을 추출하는 방법에 대해 더 알아보고 싶으시다면 아래 링크들을 확인해보세요.

- [파이썬 공식 문서 - Sequence Types](https://docs.python.org/3/library/stdtypes.html#text-sequence-type-str)  
- [Tutorialspoint - Python String Slicing](https://www.tutorialspoint.com/python/string_slice.htm)  

# 또 다른 정보
- ["제목 없음" 블로그 - 파이썬에서 문자열 슬라이싱 이해하기](https://blog.naver.com/PostView.nhn?blogId=cosmosjs&logNo=221218851517)