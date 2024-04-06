---
date: 2024-01-20 17:35:18.079864-07:00
description: "How to: (\uC5B4\uB5BB\uAC8C \uD558\uB098\uC694?) \uC61B\uB0A0\uC5D0\uB294\
  \ '+' \uC5F0\uC0B0\uC774\uB098 '%' \uC5F0\uC0B0\uC744 \uD1B5\uD574 \uBB38\uC790\uC5F4\
  \uC744 \uD569\uCCE4\uC8E0. \uD30C\uC774\uC36C 3.6\uBD80\uD130\uB294 f-string\uC774\
  \uB77C\uB294 \uB9E4\uB825\uC801\uC778 \uB3C4\uAD6C\uAC00 \uC0DD\uACBC\uC2B5\uB2C8\
  \uB2E4. f-string\uC740 \uAC00\uB3C5\uC131\uC774 \uC88B\uACE0 \uC2E4\uD589\uB3C4\
  \ \uBE60\uB974\uC8E0. \uBB38\uC790\uC5F4\uC744 \uB354\uD560 \uB54C\uB9C8\uB2E4 \uC0C8\
  \uB85C\uC6B4 \uBB38\uC790\uC5F4\uC744 \uB9CC\uB4DC\uB2C8, \uB9CE\uC740 \uC591\uC744\
  \ \uB2E4\uB8F0\u2026"
isCJKLanguage: true
lastmod: '2024-04-05T22:51:09.078310-06:00'
model: gpt-4-1106-preview
summary: "(\uC5B4\uB5BB\uAC8C \uD558\uB098\uC694?) \uC61B\uB0A0\uC5D0\uB294 '+' \uC5F0\
  \uC0B0\uC774\uB098 '%' \uC5F0\uC0B0\uC744 \uD1B5\uD574 \uBB38\uC790\uC5F4\uC744\
  \ \uD569\uCCE4\uC8E0."
title: "\uBB38\uC790\uC5F4 \uC5F0\uACB0\uD558\uAE30"
weight: 3
---

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
