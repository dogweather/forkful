---
title:                "문자열 연결하기"
aliases:
- ko/python/concatenating-strings.md
date:                  2024-01-20T17:35:18.079864-07:00
model:                 gpt-4-1106-preview
simple_title:         "문자열 연결하기"

tag:                  "Strings"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/python/concatenating-strings.md"
---

{{< edit_this_page >}}

## What & Why? (무엇과 왜?)
문자열 연결은 서로 다른 문자열을 붙이는 것입니다. 프로그래머들이 데이터를 조합하거나 출력을 형식화할 때 사용합니다.

## How to: (어떻게 하나요?)
```Python
# '+' 사용
hello = "안녕"
world = "세상"
greeting = hello + ", " + world + "!"
print(greeting)  # '안녕, 세상!'

# join() 함수 사용
name_list = ["김", "이", "박"]
full_name = "".join(name_list)
print(full_name)  # '김이박'

# f-string 사용 (Python 3.6 이상)
first_name = "홍"
last_name = "길동"
full_name = f"{first_name} {last_name}"
print(full_name)  # '홍 길동'
```

## Deep Dive (심도 있게)
옛날에는 '+' 연산이나 '%' 연산을 통해 문자열을 합쳤죠. 파이썬 3.6부터는 f-string이라는 매력적인 도구가 생겼습니다. f-string은 가독성이 좋고 실행도 빠르죠. 문자열을 더할 때마다 새로운 문자열을 만드니, 많은 양을 다룰 땐 `.join()` 같은 효율적인 방법을 사용해야 합니다. 메모리 사용이나 속도 면에서 중요합니다.

## See Also (더 보기)
- 파이썬 공식 문자열 문서: [Python String Documentation](https://docs.python.org/3/library/stdtypes.html#text-sequence-type-str)
- f-string에 대한 블로그 글: [Real Python on f-strings](https://realpython.com/python-f-strings/)
- 문자열 최적화 가이드: [Efficient String Concatenation in Python](https://waymoot.org/home/python_string/)
